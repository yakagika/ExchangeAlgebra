{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.Convert      as EC
import qualified ExchangeAlgebra.Convert.Checked as ECC
import qualified ExchangeAlgebra.Consolidation.Worksheet as CW
import qualified ExchangeAlgebra.TrialBalance.Validation as TB
import qualified ExchangeAlgebra.Reporting.Presentation as RP
import qualified ExchangeAlgebra.Reporting.Metric as RM
import qualified ExchangeAlgebra.Convert.Csv  as ECsv
import qualified ExchangeAlgebra.Assist       as Assist
import qualified ExchangeAlgebra.Assist.Descriptions as AssistDesc
import qualified ExchangeAlgebra.Algebra.Base.Account.Registry as Registry
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Algebra.Transfer as EAT
import qualified ExchangeAlgebra.Journal  as EJ
import qualified ExchangeAlgebra.Journal.Transfer as EJT
import qualified ExchangeAlgebra.Bookkeeping as EB
import           ExchangeAlgebra.Value    (MoneyDecimal, bankersRound)
import qualified ExchangeAlgebra.Simulate as ES
import           ExchangeAlgebra.Simulate
import qualified ExchangeAlgebra.Simulate.Lite as Lite
import           ExchangeAlgebra.Simulate.Network
                     ( TradeNetwork, InputCoefficients, NetworkError(..)
                     , tradeNetwork, inputCoefficients
                     , nodes, edges, suppliersOf, buyersOf, edgeCount
                     , coefficient, inputsOf, sigmaEdges
                     , completeNetwork, kRegular, erdosRenyi, scaleFree, sectorBlock
                     , CoefOptions(..), defaultCoefOptions, randomCoefficients
                     , networkFromTable, coefficientsFromTable, fromCoefficientMatrix
                     , parseEdgeCsv, parseCoefCsv )
import           ExchangeAlgebra.Simulate.Lite
                     ( InitT, RefT, SnapT, HK
                     , Field(..), carry, resetEach, updateEach
                     , Stage, stage, stageFor, stageOf
                     , Par(..), SimSpec, mkSimSpec, runLite, runLiteWithPolicy )
import qualified ExchangeAlgebra.Simulate.Policy as Policy
import           ExchangeAlgebra.Value    (MoneyDouble)
import qualified ExchangeAlgebra.Write    as EW
import           ExchangeAlgebra.Write
import qualified ExchangeAlgebra.Optimize           as O
import qualified ExchangeAlgebra.Optimize.Annealing as OA
import qualified ExchangeAlgebra.Optimize.GA        as OG
import qualified Data.Vector.Unboxed as UV

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import qualified Data.List.NonEmpty  as NE
import qualified Data.Set            as Set
import           Data.Char           (isAlphaNum, isSpace)
import qualified Data.Binary         as Binary
import qualified Data.Binary.Put     as BinaryPut
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Numeric             (showHex)
import           Control.Monad       (forM_)
import           Control.Monad.ST
import           Data.Array.ST
import           Data.STRef
import           System.Exit         (exitFailure)
import           System.IO           (IOMode(WriteMode), withFile)
import           Data.Time           (Day, TimeOfDay(..), fromGregorian)
import           System.Directory    (removeFile)
import           System.Random       (StdGen, mkStdGen, randomR, split)
import           Control.Monad       (replicateM)
import           Control.Monad.State (runState, state)
import           Control.Exception   (try, evaluate, SomeException)
import           Test.QuickCheck hiding (Fixed)
import           GHC.Generics        (Generic)
import           System.Random       (randomR)

-- ================================================================
-- Unit test helpers
-- ================================================================

eps :: Double
eps = 1e-9

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | expected == actual = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

assertNear :: String -> Double -> Double -> IO ()
assertNear label expected actual
    | abs (expected - actual) <= eps = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

decodeAccountTitleOrFail :: BL.ByteString -> Either String AccountTitles
decodeAccountTitleOrFail bytes = case Binary.decodeOrFail bytes of
    Left (_, _, message) -> Left message
    Right (_, _, title)  -> Right title

testAccountTitlesBinary :: IO ()
testAccountTitlesBinary = do
    let titles = [minBound .. maxBound] :: [AccountTitles]
        roundTripped = L.map (Binary.decode . Binary.encode) titles
        invalidTag = fromIntegral (fromEnum (maxBound :: AccountTitles) + 1)
        invalidBytes = BinaryPut.runPut (BinaryPut.putWord16be invalidTag)
    assertEqual "AccountTitles Binary covers all 233 constructors"
        233 (L.length titles)
    assertEqual "AccountTitles Binary Word16be roundtrip"
        titles roundTripped
    assertEqual "AccountTitles Binary rejects out-of-range Word16"
        True (case decodeAccountTitleOrFail invalidBytes of
            Left _  -> True
            Right _ -> False)

-- ================================================================
-- AccountTitles classification exhaustiveness (Phase A)
-- ================================================================
--
-- Pins the (whatDiv, whichSide, fixedCurrent) classification of every
-- AccountTitles constructor against an explicit expected table that encodes
-- the Phase A design table. Any new constructor that is not added here makes
-- the test fail (the [minBound .. maxBound] traversal will hit a title absent
-- from the table), forcing the table to be kept in sync and guarding against
-- classifyAccountDivision's wildcard silently classifying a title as Assets.
--
-- whichSide is evaluated on the @Not :< title@ base (no Hat reversal), so it
-- equals the "home side" implied by whatDiv: Debit for Assets/Cost,
-- Credit for Liability/Equity/Revenue.

-- | Expected classification for every non-wildcard AccountTitles constructor.
--   (title, expected whatDiv, expected whichSide on Not-base, expected fixedCurrent)
accountTitleClassTable :: [(AccountTitles, AccountDivision, Side, FixedCurrent)]
accountTitleClassTable =
    -- Pre-existing titles
    [ (Cash,                          Assets,    Debit,  Current)
    , (Deposits,                      Assets,    Debit,  Current)
    , (CurrentDeposits,               Assets,    Debit,  Current)
    , (Securities,                    Assets,    Debit,  Current)
    , (InvestmentSecurities,          Assets,    Debit,  Fixed)
    , (LongTermNationalBonds,         Assets,    Debit,  Fixed)
    , (ShortTermNationalBonds,        Assets,    Debit,  Current)
    , (Products,                      Assets,    Debit,  Current)
    , (Machinery,                     Assets,    Debit,  Fixed)
    , (Building,                      Assets,    Debit,  Fixed)
    , (Vehicle,                       Assets,    Debit,  Fixed)
    , (StockInvestment,               Assets,    Debit,  Other)
    , (EquipmentInvestment,           Assets,    Debit,  Fixed)
    , (LongTermLoansReceivable,       Assets,    Debit,  Fixed)
    , (AccountsReceivable,            Assets,    Debit,  Current)
    , (ShortTermLoansReceivable,      Assets,    Debit,  Current)
    , (ReserveDepositReceivable,      Assets,    Debit,  Current)
    , (Gold,                          Assets,    Debit,  Fixed)
    , (GovernmentService,             Assets,    Debit,  Current)
    , (CapitalStock,                  Equity,    Credit, Other)
    , (RetainedEarnings,              Equity,    Credit, Other)
    , (LongTermLoansPayable,          Liability, Credit, Fixed)
    , (ShortTermLoansPayable,         Liability, Credit, Current)
    , (LoansPayable,                  Liability, Credit, Current)
    , (ReserveForDepreciation,        Liability, Credit, Current)
    , (DepositPayable,                Liability, Credit, Current)
    , (LongTermNationalBondsPayable,  Liability, Credit, Fixed)
    , (ShortTermNationalBondsPayable, Liability, Credit, Current)
    , (ReserveDepositPayable,         Liability, Credit, Current)
    , (CentralBankNotePayable,        Liability, Credit, Current)
    , (Depreciation,                  Cost,      Debit,  Other)
    , (AmortizationExpense,           Cost,      Debit,  Other)
    , (SalesCost,                     Cost,      Debit,  Other)
    , (BusinessTrip,                  Cost,      Debit,  Other)
    , (Commutation,                   Cost,      Debit,  Other)
    , (UtilitiesExpense,              Cost,      Debit,  Other)
    , (RentExpense,                   Cost,      Debit,  Other)
    , (AdvertisingExpense,            Cost,      Debit,  Other)
    , (DeliveryExpenses,              Cost,      Debit,  Other)
    , (SuppliesExpenses,              Cost,      Debit,  Other)
    , (MiscellaneousExpenses,         Cost,      Debit,  Other)
    , (WageExpenditure,               Cost,      Debit,  Other)
    , (InterestExpense,               Cost,      Debit,  Other)
    , (TaxesExpense,                  Cost,      Debit,  Other)
    , (ConsumptionExpenditure,        Cost,      Debit,  Other)
    , (SubsidyExpense,                Cost,      Debit,  Other)
    , (CentralBankPaymentExpense,     Cost,      Debit,  Other)
    , (Purchases,                     Cost,      Debit,  Other)
    , (NetIncome,                     Cost,      Debit,  Other)
    , (ValueAdded,                    Revenue,   Credit, Other)
    , (SubsidyIncome,                 Revenue,   Credit, Other)
    , (NationalBondInterestEarned,    Revenue,   Credit, Other)
    , (DepositInterestEarned,         Revenue,   Credit, Other)
    , (GrossProfit,                   Revenue,   Credit, Other)
    , (OrdinaryProfit,                Revenue,   Credit, Other)
    , (InterestEarned,                Revenue,   Credit, Other)
    , (ReceiptFee,                    Revenue,   Credit, Other)
    , (RentalIncome,                  Revenue,   Credit, Other)
    , (WageEarned,                    Revenue,   Credit, Other)
    , (TaxesRevenue,                  Revenue,   Credit, Other)
    , (CentralBankPaymentIncome,      Revenue,   Credit, Other)
    , (Sales,                         Revenue,   Credit, Other)
    , (NetLoss,                       Revenue,   Credit, Other)
    -- Phase A additions: Assets (資産)
    , (PettyCash,                     Assets,    Debit,  Current)
    , (NotesReceivable,               Assets,    Debit,  Current)
    , (ElectronicallyRecordedReceivable, Assets, Debit,  Current)
    , (CreditCardReceivable,          Assets,    Debit,  Current)
    , (NotesLoansReceivable,          Assets,    Debit,  Current)
    , (MerchandiseInventory,          Assets,    Debit,  Current)
    , (AdvancesPaid,                  Assets,    Debit,  Current)
    , (PrepaidExpenses,               Assets,    Debit,  Current)
    , (AccruedRevenue,                Assets,    Debit,  Current)
    , (OtherReceivables,              Assets,    Debit,  Current)
    , (PaymentsOnBehalf,              Assets,    Debit,  Current)
    , (SuspensePayments,              Assets,    Debit,  Current)
    , (ConsumptionTaxPaid,            Assets,    Debit,  Current)
    , (PrepaidCorporateIncomeTaxes,   Assets,    Debit,  Current)
    , (Land,                          Assets,    Debit,  Fixed)
    , (Fixtures,                      Assets,    Debit,  Fixed)
    , (Patent,                        Assets,    Debit,  Fixed)
    , (Trademark,                     Assets,    Debit,  Fixed)
    , (Software,                      Assets,    Debit,  Fixed)
    , (CashOverShort,                 Assets,    Debit,  Other)
    -- Phase A additions: Liability (負債)
    , (AccountsPayable,               Liability, Credit, Current)
    , (NotesPayable,                  Liability, Credit, Current)
    , (ElectronicallyRecordedObligations, Liability, Credit, Current)
    , (NotesLoansPayable,             Liability, Credit, Current)
    , (BankOverdraft,                 Liability, Credit, Current)
    , (AdvancesReceived,              Liability, Credit, Current)
    , (UnearnedRevenue,               Liability, Credit, Current)
    , (AccruedExpenses,               Liability, Credit, Current)
    , (OtherPayables,                 Liability, Credit, Current)
    , (DepositsReceived,              Liability, Credit, Current)
    , (SuspenseReceipts,              Liability, Credit, Current)
    , (ConsumptionTaxReceived,        Liability, Credit, Current)
    , (AccruedConsumptionTax,         Liability, Credit, Current)
    , (AccruedCorporateIncomeTaxes,   Liability, Credit, Current)
    , (UnpaidDividends,               Liability, Credit, Current)
    , (AllowanceForDoubtfulAccounts,  Assets,    Credit, Current)  -- contra asset (isContra)
    , (AccumulatedDepreciation,       Assets,    Credit, Fixed)    -- contra asset (isContra)
    -- Phase A additions: Equity (資本)
    , (LegalRetainedEarnings,         Equity,    Credit, Other)
    -- Phase A additions: Cost (費用)
    , (ProvisionForDoubtfulAccounts,  Cost,      Debit,  Other)
    , (BadDebtLoss,                   Cost,      Debit,  Other)
    , (LossOnSalesOfFixedAssets,      Cost,      Debit,  Other)
    , (LossOnSalesOfNotesReceivable,  Cost,      Debit,  Other)
    , (PaymentFees,                   Cost,      Debit,  Other)
    , (MiscellaneousLoss,             Cost,      Debit,  Other)
    , (CorporateIncomeTaxes,          Cost,      Debit,  Other)
    , (CommunicationExpenses,         Cost,      Debit,  Other)
    -- Phase A additions: Revenue (収益)
    , (GainOnSalesOfFixedAssets,      Revenue,   Credit, Other)
    , (RecoveryOfBadDebts,            Revenue,   Credit, Other)
    , (MiscellaneousIncome,           Revenue,   Credit, Other)
    -- Phase B addition: Revenue (収益)
    , (ReversalOfAllowanceForDoubtfulAccounts, Revenue, Credit, Other)
    -- T4b additions: equity-method accounts
    , (InvestmentInAssociate,                 Assets,   Debit,  Fixed)
    , (EquityInEarningsOfInvestee,            Revenue,  Credit, Other)
    -- FX library additions: OCI/capital accounts
    , (CumulativeTranslationAdjustment,       Equity,   Credit, Other)
    -- V-Land 2 additions
    , (TimeDeposits, Assets, Debit, Current)
    , (LoansReceivable, Assets, Debit, Current)
    , (GiftCertificatesReceived, Assets, Debit, Current)
    , (SecurityDepositsPaid, Assets, Debit, Fixed)
    , (SuppliesOnHand, Assets, Debit, Current)
    , (ContractAssets, Assets, Debit, Current)
    , (IncomeTaxesRefundReceivable, Assets, Debit, Current)
    , (WorkInProcess, Assets, Debit, Current)
    , (DeferredTaxAssets, Assets, Debit, Fixed)
    , (LeasedAssets, Assets, Debit, Fixed)
    , (ToolsAndInstruments, Assets, Debit, Fixed)
    , (ConstructionInProgress, Assets, Debit, Fixed)
    , (Goodwill, Assets, Debit, Fixed)
    , (SoftwareInProgress, Assets, Debit, Fixed)
    , (LongTermPrepaidExpenses, Assets, Debit, Fixed)
    , (DishonoredNotesReceivable, Assets, Debit, Current)
    , (PrepaidPensionCost, Assets, Debit, Fixed)
    , (NetDefinedBenefitAsset, Assets, Debit, Fixed)
    , (DepositsInSpecialAccounts, Assets, Debit, Current)
    , (Structures, Assets, Debit, Fixed)
    , (LeaseholdRights, Assets, Debit, Fixed)
    , (NonOperatingNotesReceivable, Assets, Debit, Current)
    , (NonOperatingElectronicallyRecordedReceivable, Assets, Debit, Current)
    , (RefundLiabilities, Liability, Credit, Current)
    , (NonOperatingNotesPayable, Liability, Credit, Current)
    , (NonOperatingElectronicallyRecordedObligations, Liability, Credit, Current)
    , (BonusesPayable, Liability, Credit, Current)
    , (AllowanceForRepairs, Liability, Credit, Current)
    , (AllowanceForProductWarranties, Liability, Credit, Current)
    , (AllowanceForBonuses, Liability, Credit, Current)
    , (DeferredTaxLiabilities, Liability, Credit, Fixed)
    , (LeaseObligations, Liability, Credit, Fixed)
    , (GuaranteeDepositsReceived, Liability, Credit, Fixed)
    , (AllowanceForRetirementBenefits, Liability, Credit, Fixed)
    , (LongTermOtherPayables, Liability, Credit, Fixed)
    , (NetDefinedBenefitLiability, Liability, Credit, Fixed)
    , (StockSubscriptionDeposits, Equity, Credit, Other)
    , (LegalCapitalSurplus, Equity, Credit, Other)
    , (OtherCapitalSurplus, Equity, Credit, Other)
    , (DividendEqualizationReserve, Equity, Credit, Other)
    , (RepairFundReserve, Equity, Credit, Other)
    , (ConstructionFundReserve, Equity, Credit, Other)
    , (GeneralReserve, Equity, Credit, Other)
    , (ValuationDifferenceOnOtherSecurities, Equity, Credit, Other)
    , (NonControllingInterests, Equity, Credit, Other)
    , (CapitalSurplus, Equity, Credit, Other)
    , (EarnedSurplus, Equity, Credit, Other)
    , (ServiceRevenue, Revenue, Credit, Other)
    , (OperatingRevenue, Revenue, Credit, Other)
    , (GainOnSalesOfSecurities, Revenue, Credit, Other)
    , (GainOnValuationOfSecurities, Revenue, Credit, Other)
    , (DividendsReceived, Revenue, Credit, Other)
    , (InterestOnSecurities, Revenue, Credit, Other)
    , (GainOnSalesOfInvestmentSecurities, Revenue, Credit, Other)
    , (InsuranceGain, Revenue, Credit, Other)
    , (GainOnBargainPurchase, Revenue, Credit, Other)
    , (ReversalOfAllowanceForRepairs, Revenue, Credit, Other)
    , (ReversalOfAllowanceForProductWarranties, Revenue, Credit, Other)
    , (GainOnDonationOfFixedAssets, Revenue, Credit, Other)
    , (GainOnNationalSubsidies, Revenue, Credit, Other)
    , (GainOnConstructionGrants, Revenue, Credit, Other)
    , (LandRentReceived, Revenue, Credit, Other)
    , (SalesRebates, Revenue, Debit, Other)
    , (CostOfServices, Cost, Debit, Other)
    , (OperatingExpenses, Cost, Debit, Other)
    , (InventoryShrinkageLoss, Cost, Debit, Other)
    , (LossOnValuationOfMerchandise, Cost, Debit, Other)
    , (Bonuses, Cost, Debit, Other)
    , (RetirementBenefitExpenses, Cost, Debit, Other)
    , (ProvisionForRepairs, Cost, Debit, Other)
    , (ProvisionForBonuses, Cost, Debit, Other)
    , (ProvisionForProductWarranties, Cost, Debit, Other)
    , (ResearchAndDevelopmentExpenses, Cost, Debit, Other)
    , (AmortizationOfGoodwill, Cost, Debit, Other)
    , (AmortizationOfSoftware, Cost, Debit, Other)
    , (AmortizationOfPatents, Cost, Debit, Other)
    , (LeaseExpenses, Cost, Debit, Other)
    , (IncorporationExpenses, Cost, Debit, Other)
    , (StockIssuanceCosts, Cost, Debit, Other)
    , (BusinessCommencementExpenses, Cost, Debit, Other)
    , (DevelopmentExpenses, Cost, Debit, Other)
    , (LossOnSalesOfElectronicallyRecordedReceivables, Cost, Debit, Other)
    , (LossOnSalesOfReceivables, Cost, Debit, Other)
    , (LossOnSalesOfSecurities, Cost, Debit, Other)
    , (LossOnValuationOfSecurities, Cost, Debit, Other)
    , (LossOnSalesOfInvestmentSecurities, Cost, Debit, Other)
    , (LossOnFire, Cost, Debit, Other)
    , (LossOnRetirementOfFixedAssets, Cost, Debit, Other)
    , (LossOnReductionOfFixedAssets, Cost, Debit, Other)
    , (AdditionalIncomeTaxesForPriorPeriods, Cost, Debit, Other)
    , (RefundOfIncomeTaxes, Cost, Credit, Other)
    , (PurchaseRebates, Cost, Credit, Other)
    , (WelfareExpenses, Cost, Debit, Other)
    , (MaintenanceExpenses, Cost, Debit, Other)
    , (StatutoryWelfareExpenses, Cost, Debit, Other)
    , (LandRentPaid, Cost, Debit, Other)
    , (InsuranceExpense, Cost, Debit, Other)
    , (RepairsExpense, Cost, Debit, Other)
    , (StorageExpenses, Cost, Debit, Other)
    , (MembershipFees, Cost, Debit, Other)
    , (IncomeSummary, Assets, Debit, Other)
    , (SuspenseAccount, Assets, Debit, Current)
    , (ForeignExchangeGains, Revenue, Credit, Other)
    , (ForeignExchangeLosses, Cost, Debit, Other)
    , (ContraAccountForGuaranteeObligations, Assets, Debit, Other)
    , (GuaranteeObligations, Liability, Credit, Other)
    , (IncomeTaxesAdjustment, Cost, Debit, Other)
    , (BranchCurrentAccount, Assets, Debit, Other)
    , (HeadOfficeCurrentAccount, Liability, Credit, Other)
    , (NetIncomeAttributableToNCI, Cost, Debit, Other)
    , (NetLossAttributableToNCI, Revenue, Credit, Other)
    , (TradingSecurities, Assets, Debit, Current)
    , (HeldToMaturityBonds, Assets, Debit, Fixed)
    , (SubsidiaryStocks, Assets, Debit, Fixed)
    , (AffiliateStocks, Assets, Debit, Fixed)
    , (AvailableForSaleSecurities, Assets, Debit, Fixed)
    ]

testAccountTitleClassification :: IO ()
testAccountTitleClassification = do
    -- All non-wildcard constructors, derived from Bounded/Enum.
    let allTitles  = [ t | t <- [minBound .. maxBound], t /= AccountTitle ]
        tableMap   = M.fromList [ (t, (d, s, fc)) | (t, d, s, fc) <- accountTitleClassTable ]
        -- A title is "covered" iff it appears in the expected table.
        missing    = [ t | t <- allTitles, not (M.member t tableMap) ]
        extra      = [ t | (t, _, _, _) <- accountTitleClassTable, t `notElem` allTitles ]
    -- Guard: the table must list exactly the non-wildcard constructors.
    assertEqual "AccountTitles class table covers every constructor (no missing)"
        ([] :: [AccountTitles]) missing
    assertEqual "AccountTitles class table has no stale entry (no extra)"
        ([] :: [AccountTitles]) extra
    -- Per-title classification must match the expected table.
    forM_ allTitles $ \t -> do
        let base    = Not :< t :: HatBase AccountTitles
            actual  = (whatDiv base, whichSide base, fixedCurrent base)
        case M.lookup t tableMap of
            Just expected ->
                assertEqual ("classification of " ++ show t) expected actual
            Nothing -> return ()  -- already reported by the "missing" guard

type TestAlg = EA.Alg Double (HatBase CountUnit)
type TestJournal = EJ.Journal String Double (HatBase CountUnit)
type AxisJournal = EJ.Journal (String, Int) Double (HatBase CountUnit)

algSample :: TestAlg
algSample =
       (1 :@ (Hat    :< Yen))
    .+ (1 :@ (Not    :< Amount))
    .+ (2 :@ (Not    :< Yen))
    .+ (2 :@ (Hat    :< Amount))
    .+ (3 :@ (Hat    :< Yen))

journalSample :: TestJournal
journalSample = EJ.fromList [x, y, z]
  where
    x = ((1 :@ (Hat :< Yen)) .+ (1 :@ (Not :< Amount))) .| "cat"  :: TestJournal
    y = ((2 :@ (Not :< Yen)) .+ (2 :@ (Hat :< Amount))) .| "dog"  :: TestJournal
    z = ((3 :@ (Hat :< Yen)) .+ (3 :@ (Not :< Amount))) .| "fish" :: TestJournal

-- | Multi-pattern 'proj' uses __set__ semantics: a duplicated query selects the
-- same posting only once (no double counting). The de-duplicated query list and
-- its de-duplicated counterpart must give identical results.
testProjMultiPatternOnePass :: IO ()
testProjMultiPatternOnePass = do
    let qs, qsDedup :: [HatBase CountUnit]
        qs      = [Hat :< Yen, HatNot :< Amount, Hat :< Yen]   -- Hat:<Yen duplicated
        qsDedup = [Hat :< Yen, HatNot :< Amount]
    -- duplicate query does not change the projection (set semantics)
    assertEqual "Alg.proj treats query list as a set (duplicate exact)"
        (EA.proj qsDedup algSample) (EA.proj qs algSample)
    -- and the duplicated Hat:<Yen is counted once, not twice
    assertEqual "Alg.proj no double counting (single Hat:<Yen)"
        (EA.proj [Hat :< Yen] algSample)
        (EA.proj [Hat :< Yen, Hat :< Yen] algSample)

-- | 'projNetNorm' returns a bar-netted norm; the identity is
-- @projNetNorm bs x == norm (bar (proj bs x))@ (not @norm (proj bs x)@), and the
-- query list is a set (duplicates do not double count).
testProjNormFastPath :: IO ()
testProjNormFastPath = do
    let qs :: [HatBase CountUnit]
        qs = [Hat :< Yen, HatNot :< Amount, Hat :< Yen]
        expected = norm $ EA.bar $ EA.proj qs algSample
        actual = EA.projNetNorm qs algSample
    assertNear "Alg.projNetNorm == norm . bar . proj (set semantics)" expected actual

-- | R7 sentinel (a): a duplicated exact base must project the same as a single
-- copy (MoneyDecimal exact: no floating tolerance needed).
testProjDuplicateExact :: IO ()
testProjDuplicateExact = do
    let alg :: EA.Alg MoneyDecimal (HatBase CountUnit)
        alg =  (10 :@ (Hat :< Yen))
            .+ (3  :@ (Not :< Amount))
        b = Hat :< Yen :: HatBase CountUnit
    assertEqual "proj [b,b] == proj [b] (duplicate exact, MoneyDecimal)"
        (EA.proj [b] alg) (EA.proj [b, b] alg)
    assertEqual "projNetNorm [b,b] == projNetNorm [b] (duplicate exact)"
        (EA.projNetNorm [b] alg) (EA.projNetNorm [b, b] alg)

-- | R7 sentinel (b): an exact base together with a wildcard query that subsumes
-- it must not double count the overlapping posting.
testProjExactWildcardOverlap :: IO ()
testProjExactWildcardOverlap = do
    let alg :: EA.Alg MoneyDecimal (HatBase CountUnit)
        alg =  (10 :@ (Hat :< Yen))
            .+ (5  :@ (Not :< Amount))
        exact = Hat :< Yen     :: HatBase CountUnit
        wild  = Hat :< (.#)    :: HatBase CountUnit   -- subsumes Hat:<Yen
    -- the wildcard already selects everything the exact base does, so the union
    -- equals the wildcard alone (overlap counted once)
    assertEqual "proj [exact,wild] == proj [wild] (overlap, no double count)"
        (EA.proj [wild] alg) (EA.proj [exact, wild] alg)
    assertEqual "projNetNorm [exact,wild] == projNetNorm [wild] (overlap)"
        (EA.projNetNorm [wild] alg) (EA.projNetNorm [exact, wild] alg)

-- | R7 sentinel (c): the bar-netted identity @projNetNorm bs x == norm (bar (proj
-- bs x))@ holds on a base carrying both hat and not sides (where it differs from
-- @norm (proj bs x)@).
testProjNormBarIdentity :: IO ()
testProjNormBarIdentity = do
    let alg :: EA.Alg MoneyDecimal (HatBase CountUnit)
        alg =  (10 :@ (Hat :< Yen))     -- Yen carries both sides
            .+ (4  :@ (Not :< Yen))
            .+ (7  :@ (Not :< Amount))
        bs = [HatNot :< Yen, HatNot :< Amount] :: [HatBase CountUnit]
    assertEqual "projNetNorm == norm . bar . proj (both-sided base, MoneyDecimal)"
        (norm (EA.bar (EA.proj bs alg))) (EA.projNetNorm bs alg)

testProjWithBaseNorm :: IO ()
testProjWithBaseNorm = do
    let bs :: [HatBase CountUnit]
        bs = [Not :< Amount]
        expected = norm $ EJ.projWithBase bs journalSample
        actual = EJ.projWithBaseNetNorm bs journalSample
    assertNear "Journal.projWithBaseNetNorm matches norm . projWithBase" expected actual

testProjWithNoteNorm :: IO ()
testProjWithNoteNorm = do
    let bs :: [HatBase CountUnit]
        bs = [HatNot :< Amount, Hat :< Yen]
        ns1 = ["dog", "cat"]
        ns2 = [plank]
        expected1 = norm $ EJ.projWithNoteBase ns1 bs journalSample
        actual1 = EJ.projWithNoteBaseNetNorm ns1 bs journalSample
        expected2 = norm $ EJ.projWithNoteBase ns2 bs journalSample
        actual2 = EJ.projWithNoteBaseNetNorm ns2 bs journalSample
    assertNear "Journal.projWithNoteBaseNetNorm (selected notes)" expected1 actual1
    assertNear "Journal.projWithNoteBaseNetNorm (plank wildcard)" expected2 actual2

-- | Sentinel for the REMOVED RULES rewrite
-- @norm (projWithBase bs js) = projWithBaseNetNorm bs js@ (and the note-base
-- analogue): the equation is false when a query selects both sides of one
-- base. 'EJ.projWithBaseNetNorm' \/ 'EJ.projWithNoteBaseNetNorm' are the /bar-netted/
-- read-outs (per base @|not - hat|@), while @norm . projWithBase@ is the
-- gross norm (sums both sides). Both values are pinned here so a future
-- \"optimization\" that silently nets the gross path fails loudly.
testProjWithBaseNormBothSided :: IO ()
testProjWithBaseNormBothSided = do
    let alg :: EA.Alg MoneyDecimal (HatBase CountUnit)
        alg =  (10 :@ (Hat :< Yen))     -- Yen carries both sides
            .+ (4  :@ (Not :< Yen))
            .+ (7  :@ (Not :< Amount))
        js = alg .| "n" :: EJ.Journal String MoneyDecimal (HatBase CountUnit)
        bs = [HatNot :< Yen] :: [HatBase CountUnit]
    assertEqual "projWithBaseNetNorm nets both sides (HatNot query): |10-4|"
        6 (EJ.projWithBaseNetNorm bs js)
    assertEqual "norm . projWithBase stays gross (no RULES rewrite): 10+4"
        14 (norm (EJ.projWithBase bs js))
    assertEqual "projWithBaseNetNorm == norm . map bar . projWithBase"
        (norm (EJ.map EA.bar (EJ.projWithBase bs js)))
        (EJ.projWithBaseNetNorm bs js)
    assertEqual "projWithNoteBaseNetNorm nets both sides (HatNot query): |10-4|"
        6 (EJ.projWithNoteBaseNetNorm ["n"] bs js)
    assertEqual "norm . projWithNoteBase stays gross (no RULES rewrite): 10+4"
        14 (norm (EJ.projWithNoteBase ["n"] bs js))

-- | Regression test for the `bases` typo bug.
--
-- Before the fix at Algebra.hs:868, `bases` ignored the `_notSide` Seq and
-- iterated `_hatSide` twice (with `Hat` and `Not` labels). As a result,
-- `length (bases x) != length (vals x)` whenever Hat/Not Seq lengths differed.
--
-- This test constructs an Alg where the Hat Seq for `Yen` has length 1 and
-- the Not Seq has length 2, plus a separate basis whose Hat Seq is empty.
-- That makes the divergence detectable in both directions.
testBasesNotSideRegression :: IO ()
testBasesNotSideRegression = do
    let alg :: TestAlg
        alg =  (100 :@ (Hat :< Yen))      -- Yen: hatSide = [100]
            .+ (50  :@ (Not :< Yen))      -- Yen: notSide = [50]
            .+ (30  :@ (Not :< Yen))      -- Yen: notSide = [50, 30]
            .+ (20  :@ (Not :< Amount))   -- Amount: notSide = [20], hatSide = []
        vs = EA.vals alg
        bs = EA.bases alg
        hatCount = length (L.filter isHat bs)
        notCount = length (L.filter (not . isHat) bs)
    -- vals and bases must agree on total count (one label per scalar entry)
    assertEqual "bases/vals same length (regression for hs/ns typo)"
        (length vs) (length bs)
    -- Expected: 1 Hat label (Hat:<Yen) and 3 Not labels (50:<Yen, 30:<Yen, 20:<Amount)
    assertEqual "bases Hat label count" 1 hatCount
    assertEqual "bases Not label count" 3 notCount

testSigmaMergePath :: IO ()
testSigmaMergePath = do
    let xs = [1 .. 5 :: Int]
        f :: Int -> TestAlg
        f i
            | i == 3 = EA.Zero
            | odd i = fromIntegral i :@ (Hat :< Yen)
            | otherwise = fromIntegral i :@ (Not :< Amount)
        expected :: TestAlg
        expected = EA.unionsMerge (L.map f xs)
        actual :: TestAlg
        actual = EA.sigma xs f
    assertEqual "Alg.sigma bulk-merge path matches unionsMerge" expected actual

-- | Characterization: the same-base 'Seq' order is __construction-path
-- dependent__. The pairwise-union path ('EA.fromList' = 'mconcat') and the
-- bulk-merge path ('EA.sigma' \/ 'EA.unionsMerge') produce the same /multiset/
-- of postings but in different sequence orders, which 'Eq' \/ @Binary@ observe
-- (and 'Double' observes through the last ULP of 'norm'\/'bar' association).
-- This test pins the current orders so any change to either path is a
-- conscious decision; unifying the paths is tracked in the 0.5.0.0 cleanup
-- plan. For order-independent comparison use `MoneyDecimal` (exact) or compare
-- after 'EA.compress'\/'EA.bar'.
testSameBaseSeqOrderPathDependence :: IO ()
testSameBaseSeqOrderPathDependence = do
    let f :: Int -> TestAlg
        f i = fromIntegral i :@ (Hat :< Yen)
        xs = L.map f [1, 2, 3]
        viaFromList = EA.fromList xs
        viaSigma    = EA.sigma [1, 2, 3] f
        viaMerge    = EA.unionsMerge xs
    assertEqual "fromList same-base seq order (pairwise-union path)"
        [3, 1, 2] (EA.vals viaFromList)
    assertEqual "sigma same-base seq order (bulk-merge path)"
        [3, 2, 1] (EA.vals viaSigma)
    assertEqual "unionsMerge order matches sigma (same merge path)"
        (EA.vals viaSigma) (EA.vals viaMerge)
    -- same multiset, different order: Eq observes the redundancy order
    assertEqual "fromList /= sigma under Eq (order is observable)"
        False (viaFromList == viaSigma)
    -- the algebraic content is nevertheless identical
    assertNear "norm agrees across construction paths"
        (norm viaFromList) (norm viaSigma)
    assertEqual "bar agrees across construction paths"
        (EA.bar viaFromList) (EA.bar viaSigma)

testSigma2When :: IO ()
testSigma2When = do
    let xs = [1 .. 3 :: Int]
        ys = [1 .. 4 :: Int]
        cond i j = i /= j && even (i + j)
        f :: Int -> Int -> TestAlg
        f i j =
            let v = fromIntegral (i * 10 + j)
            in if odd i
                then v :@ (Hat :< Yen)
                else v :@ (Not :< Amount)
        expected :: TestAlg
        expected =
            EA.unionsMerge
                [ f i j
                | i <- xs
                , j <- ys
                , cond i j
                ]
        actual :: TestAlg
        actual = EA.sigma2When xs ys cond f
    assertEqual "Alg.sigma2When matches list-comprehension sum" expected actual

testSigmaFromMap :: IO ()
testSigmaFromMap = do
    let kvs = M.fromList
            [ ((1, 2), 5.0)
            , ((2, 3), 0.0)
            , ((3, 1), 7.0)
            ] :: M.Map (Int, Int) Double
        f :: (Int, Int) -> Double -> TestAlg
        f (i, j) v
            | i < j = v :@ (Hat :< Yen)
            | otherwise = v :@ (Not :< Amount)
        expected :: TestAlg
        expected = EA.unionsMerge
            [ f (1, 2) 5.0
            , f (3, 1) 7.0
            ]
        actual :: TestAlg
        actual = EA.sigmaFromMap kvs f
    assertEqual "Alg.sigmaFromMap iterates non-zero map entries only" expected actual

testJournalFromListStrict :: IO ()
testJournalFromListStrict = do
    -- fromList is now a strict left fold (L.foldl' (.+) mempty). Verify it still
    -- preserves the posting multiset by matching the old lazy right-fold reference
    -- (foldr (.+) mempty). Colliding note keys (i `mod` 30) force same-note/same-base
    -- postings into one Alg sequence, where the two folds accumulate in opposite
    -- order; with MoneyDecimal (exact, associative) the aggregate (norm) is identical.
    let mk i = ((fromIntegral (i `mod` 7 + 1) :: MoneyDecimal)
                  :@ ((if even i then Hat else Not) :< ([Yen, Amount] !! (i `mod` 2))))
               .| show (i `mod` 30)
        xs :: [Journal String MoneyDecimal (HatBase CountUnit)]
        xs = [ mk i | i <- [1 .. 400 :: Int] ]
        strict  = EJ.fromList xs
        lazyRef = foldr (.+) mempty xs
    -- exact value type ⇒ norm identical regardless of seq order (multiset preserved)
    assertEqual "Journal.fromList (strict): norm matches lazy foldr reference (MoneyDecimal exact)"
        (norm strict) (norm lazyRef)
    -- distinct note keys ⇒ no seq collision ⇒ exact structural equality with foldr
    let ys :: [Journal String MoneyDecimal (HatBase CountUnit)]
        ys = [ ((fromIntegral i :: MoneyDecimal) :@ (Not :< Yen)) .| show i
             | i <- [1 .. 20 :: Int] ]
    assertEqual "Journal.fromList (strict): structurally equal to foldr for distinct notes"
        (EJ.toMap (EJ.fromList ys)) (EJ.toMap (foldr (.+) mempty ys))

-- | Regression test for the @union@ zero-singleton base-relabel bug
-- (Algebra.hs). When one operand of @(.+)@ is a /zero-valued/ singleton on base
-- @b1@ and the other a /real/ singleton on a different base @b2@, the result must
-- keep the real value on its OWN base (@v2:@b2@), not relabel it onto the zero
-- posting's base. The old code returned @v2:@b1@ / @v1:@b2@, which silently moved
-- a value to the wrong base. It preserved @norm@ (total unchanged) but corrupted
-- per-base projection, and surfaced as construction-order-dependent simulation
-- results (sparsified coefficients build explicit @0:@base@ singletons via raw
-- @(:@)@). See plans/in-progress/SELECTABLE_VALUE_TYPE_PLAN.md (Stage D).
testUnionZeroSingletonBase :: IO ()
testUnionZeroSingletonBase = do
    let zb = 0 :@ (Hat :< Yen)    :: TestAlg   -- zero value, base Yen
        rb = 5 :@ (Hat :< Amount) :: TestAlg   -- real value, base Amount
    -- both fold directions of the singleton/singleton union
    assertEqual "union zero(.+)real keeps real value on its own base"
        rb (EA.proj [Hat :< Amount] (zb .+ rb))
    assertEqual "union real(.+)zero keeps real value on its own base"
        rb (EA.proj [Hat :< Amount] (rb .+ zb))
    -- the real value must NOT appear on the zero posting's base
    assertEqual "union zero(.+)real: nothing relabeled onto the zero's base"
        (EA.Zero :: TestAlg) (EA.proj [Hat :< Yen] (zb .+ rb))
    assertEqual "union real(.+)zero: nothing relabeled onto the zero's base"
        (EA.Zero :: TestAlg) (EA.proj [Hat :< Yen] (rb .+ zb))

-- | Regression for audit divergence C: scalar product (.*) must reject a
-- negative / non-finite scalar instead of silently producing negative
-- (out-of-domain) postings. (Pre-fix, (.*) used raw (:@) and bypassed the
-- isErrorValue check that (.@) performs.)
testScalarRejectsNegative :: IO ()
testScalarRejectsNegative = do
    let xD = 10 :@ (Not :< Yen) :: TestAlg
    rD <- try (evaluate (norm ((-1) .* xD))) :: IO (Either SomeException Double)
    case rD of
        Left _  -> putStrLn "[PASS] (.*) rejects negative scalar (Double)"
        Right v -> do putStrLn ("[FAIL] (.*) negative scalar leaked (Double): " ++ show v); exitFailure
    let xN = 10 :@ (Not :< Yen) :: EA.Alg MoneyDecimal (HatBase CountUnit)
    rN <- try (evaluate (norm ((-1) .* xN))) :: IO (Either SomeException MoneyDecimal)
    case rN of
        Left _  -> putStrLn "[PASS] (.*) rejects negative scalar (MoneyDecimal)"
        Right v -> do putStrLn ("[FAIL] (.*) negative scalar leaked (MoneyDecimal): " ++ show v); exitFailure
    -- non-negative scalar still works
    assertNear "(.*) non-negative scalar works" 20.0 (norm (2 .* xD))

-- Step 1 (concrete projection keeps the axis index lazy): the module is compiled
-- @Strict@, so a concrete (non-wildcard) 'projNetNorm' must NOT force the lazy
-- @_axisPosting@ index (it should be a plain 'Map.lookup'); a wildcard 'projNetNorm'
-- must use (force) it. We poison the index fields with 'error' and check which
-- projection crashes. Guards the projExactMap/projWildMap split.
testProjConcreteNoIndexForce :: IO ()
testProjConcreteNoIndexForce = do
    let alg :: EA.Alg Double SimHatBase2
        alg = EA.fromList [ 10 :@ Not :< (Cash, 1, 1, Yen)
                          , 20 :@ Not :< (Products, 2, 2, Amount)
                          , 30 :@ Hat :< (Cash, 3, 3, Yen) ]
    case alg of
      EA.Liner m _ _ _ _ _ -> do
        let poison = EA.Liner m (error "POISON") (error "POISON")
                                (error "POISON") (error "POISON") (error "POISON")
        rc <- try (evaluate (EA.projNetNorm [Not :< (Cash, 1, 1, Yen)] poison))
                :: IO (Either SomeException Double)
        case rc of
          Right v | v == 10.0 -> putStrLn "[PASS] concrete projNetNorm does not force the axis index"
          Right v             -> do putStrLn ("[FAIL] concrete projNetNorm wrong value: " ++ show v); exitFailure
          Left _              -> do putStrLn "[FAIL] concrete projNetNorm forced the (poisoned) axis index"; exitFailure
        rw <- try (evaluate (EA.projNetNorm [Not :< (Cash, (.#), 1, Yen)] poison))
                :: IO (Either SomeException Double)
        case rw of
          Left _   -> putStrLn "[PASS] wildcard projNetNorm uses the axis index (forced, as required)"
          Right v  -> do putStrLn ("[FAIL] wildcard projNetNorm did not use the index: " ++ show v); exitFailure
      _ -> do putStrLn "[FAIL] expected a Liner"; exitFailure

-- The Liner @_bpToId@ and @_nextBpId@ fields are reserved for the dormant P1a
-- incremental-id scheme and are not maintained by 'linerFromMap' (it leaves them
-- as lazy 'error' poison). This guards two invariants: (1) normal projection
-- (concrete + wildcard) never forces those poisoned fields, so 'projWildMap'
-- stays green; (2) forcing the unused fields fails loudly (as designed) rather
-- than returning a stale/empty value.
testLinerReservedFieldsPoisoned :: IO ()
testLinerReservedFieldsPoisoned = do
    let alg :: EA.Alg Double SimHatBase2
        alg = EA.fromList [ 10 :@ Not :< (Cash, 1, 1, Yen)
                          , 20 :@ Not :< (Products, 2, 2, Amount)
                          , 30 :@ Hat :< (Cash, 3, 3, Yen) ]
    -- projWildMap path (and concrete path) must stay green without forcing the
    -- reserved fields.
    assertNear "wildcard projNetNorm green with reserved fields unmaintained"
        10.0 (EA.projNetNorm [Not :< (Cash, (.#), 1, Yen)] alg)
    assertNear "concrete projNetNorm green with reserved fields unmaintained"
        10.0 (EA.projNetNorm [Not :< (Cash, 1, 1, Yen)] alg)
    -- Forcing _bpToId / _nextBpId must error (poison), proving they are not
    -- silently maintained.
    case alg of
      EA.Liner _ _ bpToId _ nextBpId _ -> do
        rb <- (try (evaluate (HM.size bpToId)) :: IO (Either SomeException Int))
        case rb of
          Left _  -> putStrLn "[PASS] _bpToId is poisoned (forcing it errors as designed)"
          Right _ -> do putStrLn "[FAIL] _bpToId was forced without error (unexpectedly maintained)"; exitFailure
        rn <- (try (evaluate nextBpId) :: IO (Either SomeException Int))
        case rn of
          Left _  -> putStrLn "[PASS] _nextBpId is poisoned (forcing it errors as designed)"
          Right _ -> do putStrLn "[FAIL] _nextBpId was forced without error (unexpectedly maintained)"; exitFailure
      _ -> do putStrLn "[FAIL] expected a Liner"; exitFailure

testJournalSigmaMergePath :: IO ()
testJournalSigmaMergePath = do
    let xs = [1 .. 4 :: Int]
        f :: Int -> TestJournal
        f i = case i of
            1 -> (1 :@ (Hat :< Yen)) .| "A"
            2 -> EJ.Zero
            3 -> (EA.Zero :: TestAlg) .| "A"
            _ -> (2 :@ (Not :< Amount)) .| "B"
        expected :: TestJournal
        expected = EJ.fromMap $ HM.fromList
            [ ("A", 1 :@ (Hat :< Yen))
            , ("B", 2 :@ (Not :< Amount))
            ]
        actual = EJ.sigma xs f
    assertEqual "Journal.sigma bulk-merge path skips zero postings" (EJ.toMap expected) (EJ.toMap actual)

testJournalSigma2When :: IO ()
testJournalSigma2When = do
    let xs = [1 .. 3 :: Int]
        ys = [1 .. 3 :: Int]
        cond i j = i < j
        f :: Int -> Int -> TestJournal
        f i j
            | i == 1 && j == 2 = (EA.Zero :: TestAlg) .| "N"
            | odd (i + j) = (fromIntegral (i + j) :@ (Hat :< Yen)) .| "N"
            | otherwise = EJ.Zero
        expected :: TestJournal
        expected = EJ.fromMap $ HM.fromList [("N", 5 :@ (Hat :< Yen))]
        actual = EJ.sigma2When xs ys cond f
    assertEqual "Journal.sigma2When matches filtered pair sum" (EJ.toMap expected) (EJ.toMap actual)

testJournalSigmaOn :: IO ()
testJournalSigmaOn = do
    let xs = [1 .. 4 :: Int]
        f :: Int -> TestAlg
        f i
            | i <= 2 = EA.Zero
            | otherwise = fromIntegral i :@ (Hat :< Yen)
        expected :: TestJournal
        expected = (EA.sigma xs f) .| "SalesPurchase"
        actual :: TestJournal
        actual = EJ.sigmaOn "SalesPurchase" xs f
        zeroExpected = EJ.Zero :: TestJournal
        zeroActual = EJ.sigmaOn "SalesPurchase" xs (\_ -> EA.Zero :: TestAlg)
    assertEqual "Journal.sigmaOn attaches note after EA.sigma" (EJ.toMap expected) (EJ.toMap actual)
    assertEqual "Journal.sigmaOn returns Zero when EA.sigma is Zero" (EJ.toMap zeroExpected) (EJ.toMap zeroActual)

testJournalSigmaOnFromMap :: IO ()
testJournalSigmaOnFromMap = do
    let kvs = M.fromList
            [ ((1, 2), 4.0)
            , ((2, 3), 0.0)
            , ((2, 1), 6.0)
            ] :: M.Map (Int, Int) Double
        f :: (Int, Int) -> Double -> TestAlg
        f (i, j) v
            | i < j = v :@ (Hat :< Yen)
            | otherwise = v :@ (Not :< Amount)
        expected :: TestJournal
        expected = (EA.sigmaFromMap kvs f) .| "SalesPurchase"
        actual :: TestJournal
        actual = EJ.sigmaOnFromMap "SalesPurchase" kvs f
        zeroActual :: TestJournal
        zeroActual = EJ.sigmaOnFromMap "SalesPurchase" (M.singleton (1, 1) 0.0) f
    assertEqual "Journal.sigmaOnFromMap matches EA.sigmaFromMap + note" (EJ.toMap expected) (EJ.toMap actual)
    assertEqual "Journal.sigmaOnFromMap returns Zero for empty-effective map" (EJ.toMap (EJ.Zero :: TestJournal)) (EJ.toMap zeroActual)

testFilterByAxisEquivalent :: IO ()
testFilterByAxisEquivalent = do
    let ledger :: AxisJournal
        ledger = EJ.fromList
            [ (10 :@ (Hat :< Yen)) .| ("A", 1)
            , (20 :@ (Not :< Amount)) .| ("B", 1)
            , (30 :@ (Hat :< Yen)) .| ("A", 2)
            ]
        expected = EJ.filterWithNote (\(_, t') _ -> t' == 1) ledger
        actual = EJ.filterByAxis 1 (EJ.NoteAxisKey (1 :: Int)) ledger
        mismatch = EJ.filterByAxis 1 (EJ.NoteAxisKey ("1" :: String)) ledger
    assertEqual "Journal.filterByAxis matches filterWithNote on axis=1"
        (EJ.toMap expected)
        (EJ.toMap actual)
    assertEqual "Journal.filterByAxis type mismatch returns empty"
        (EJ.toMap (EJ.Zero :: AxisJournal))
        (EJ.toMap mismatch)

testFilterByAxisWithDeltaUpdates :: IO ()
testFilterByAxisWithDeltaUpdates = do
    let base :: AxisJournal
        base = EJ.fromMap $ HM.fromList
            [ (("A", 1), 10 :@ (Hat :< Yen))
            , (("C", 2), 5 :@ (Not :< Amount))
            ]
        rhs :: AxisJournal
        rhs = EJ.fromMap $ HM.fromList
            [ (("A", 1), 3 :@ (Not :< Amount))
            , (("B", 1), 7 :@ (Hat :< Yen))
            ]
        ledger = base .+ rhs
        expected = EJ.filterWithNote (\(_, t') _ -> t' == 1) ledger
        actual = EJ.filterByAxis 1 (EJ.NoteAxisKey (1 :: Int)) ledger
    assertEqual "Journal.filterByAxis works after append updates"
        (EJ.toMap expected)
        (EJ.toMap actual)

-- ================================================================
-- Transfer regression tests
-- ================================================================

type TransferAlg = EA.Alg Double SimHatBase2
type TransferJournal = EJ.Journal String Double SimHatBase2

transferAlgSample :: TransferAlg
transferAlgSample = EA.fromList
    [ 7  :@ Not :<(WageExpenditure, 1, 1, Yen)
    , 3  :@ Hat :<(Depreciation, 2, 2, Yen)
    , 11 :@ Not :<(Purchases, 3, 3, Yen)
    , 13 :@ Not :<(ValueAdded, 1, 2, Yen)
    , 17 :@ Hat :<(Sales, 2, 1, Yen)
    , 19 :@ Not :<(InterestEarned, 4, 4, Yen)
    , 23 :@ Hat :<(InterestExpense, 5, 5, Yen)
    , 29 :@ Not :<(TaxesRevenue, 2, 2, Yen)
    , 31 :@ Hat :<(TaxesExpense, 3, 3, Yen)
    , 37 :@ Not :<(WageEarned, 6, 6, Yen)
    , 41 :@ Hat :<(ConsumptionExpenditure, 6, 6, Yen)
    , 43 :@ Not :<(CentralBankPaymentIncome, 1, 1, Yen)
    , 47 :@ Hat :<(CentralBankPaymentExpense, 1, 1, Yen)
    , 53 :@ Not :<(GrossProfit, 7, 7, Yen)
    , 59 :@ Hat :<(OrdinaryProfit, 8, 8, Yen)
    , 61 :@ Not :<(Cash, 1, 1, Yen)
    ]

transferJournalSample :: TransferJournal
transferJournalSample = EJ.fromList
    [ transferAlgSample .| "A"
    , ((5 :@ Not :<(Sales, 2, 1, Yen)) .+ (2 :@ Hat :<(WageExpenditure, 1, 1, Yen))) .| "B"
    , ((3 :@ Hat :<(TaxesExpense, 3, 3, Yen)) .+ (4 :@ Not :<(InterestEarned, 4, 4, Yen))) .| "C"
    ]

testFinalStockTransferAlgEquivalence :: IO ()
testFinalStockTransferAlgEquivalence = do
    let ref =
            (.-)
                . EAT.retainedEarningTransfer
                . EAT.ordinaryProfitTransfer
                . EAT.grossProfitTransfer
                $ transferAlgSample
        actual = EAT.finalStockTransfer transferAlgSample
    assertEqual "Algebra.finalStockTransfer matches composed transfer" ref actual

testFinalStockTransferJournalEquivalence :: IO ()
testFinalStockTransferJournalEquivalence = do
    let ref =
            (.-)
                . EJT.retainedEarningTransfer
                . EJT.ordinaryProfitTransfer
                . EJT.grossProfitTransfer
                $ transferJournalSample
        actual = EJT.finalStockTransfer transferJournalSample
    assertEqual "Journal.finalStockTransfer matches composed transfer" (EJ.toMap ref) (EJ.toMap actual)

type FinalStockProbe = EA.Alg Double (HatBase AccountTitles)

-- | Classify the observable image of the same one-posting probe used to
-- generate @test/fixtures/pre-vocab/finalstock.tsv@.
--
-- Complexity: O(1)
finalStockProbeRule :: AccountTitles -> String
finalStockProbeRule RetainedEarnings = "SELF"
finalStockProbeRule title
    | actual == show probe = "Nothing"
    | actual == show (1 .@ Not :< RetainedEarnings :: FinalStockProbe) = "Keep"
    | actual == show (1 .@ Hat :< RetainedEarnings :: FinalStockProbe) = "Flip"
    | otherwise = "UNEXPECTED:" ++ actual
  where
    probe = 1 .@ Not :< title :: FinalStockProbe
    actual = show (EAT.finalStockTransfer probe)

-- | The pre-vocabulary fixture is frozen. The only permitted behavioural
-- changes are the concrete Cost/Revenue accounts that the former SNA-era
-- title case split omitted. Aggregate NetIncome/NetLoss remain explicit
-- registry overrides and therefore do not occur in this list.
finalStockExpectedClosedDiff :: [AccountTitles]
finalStockExpectedClosedDiff =
    [ AmortizationExpense
    , SalesCost
    , BusinessTrip
    , Commutation
    , UtilitiesExpense
    , RentExpense
    , AdvertisingExpense
    , DeliveryExpenses
    , SuppliesExpenses
    , MiscellaneousExpenses
    , NationalBondInterestEarned
    , DepositInterestEarned
    , ReceiptFee
    , RentalIncome
    , EquityInEarningsOfInvestee
    , ProvisionForDoubtfulAccounts
    , BadDebtLoss
    , LossOnSalesOfFixedAssets
    , LossOnSalesOfNotesReceivable
    , PaymentFees
    , MiscellaneousLoss
    , CorporateIncomeTaxes
    , CommunicationExpenses
    , GainOnSalesOfFixedAssets
    , RecoveryOfBadDebts
    , MiscellaneousIncome
    , ReversalOfAllowanceForDoubtfulAccounts
    ]

-- V-Land 1: finalStockRule の全域を独立参照式 (division + contra を明示分岐)
-- と突き合わせ, 方向 (Keep/Flip) まで固定する。contra P/L (将来の売上割戻等)
-- では division 基準と逆になることをこの式が明文化する。
testFinalStockRuleReference :: IO ()
testFinalStockRuleReference = mapM_ check Registry.concreteAccountTitles
  where
    check RetainedEarnings = pure ()
    check t = assertEqual ("finalStockRule reference: " ++ show t)
        (expected t) (finalStockProbeRule t)
    expected t
        | t `L.elem`
            [ NetIncome
            , NetLoss
            , IncomeSummary
            , NetIncomeAttributableToNCI
            , NetLossAttributableToNCI
            ] = "Nothing"
        | contra && div_ == Revenue = "Flip"
        | contra && div_ == Cost    = "Keep"
        | div_ == Revenue           = "Keep"
        | div_ == Cost              = "Flip"
        | otherwise                 = "Nothing"
      where
        div_   = classifyAccountDivision t
        contra = Registry.classifyAccountContra t

testFinalStockRegistryClosedDiff :: IO ()
testFinalStockRegistryClosedDiff = do
    fixture <- TIO.readFile "test/fixtures/pre-vocab/finalstock.tsv"
    let fixtureLines =
            [ line
            | line <- T.lines fixture
            , not (T.null line)
            , not (T.isPrefixOf (T.pack "#") line)
            ]
        parseFixtureLine line = case T.splitOn (T.pack "\t") line of
            [titleText, oldRule, _division] -> case EC.parseAccountTitle titleText of
                Right title -> (title, T.unpack oldRule)
                Left err -> error ("invalid final-stock fixture title: " ++ show err)
            fields -> error ("invalid final-stock fixture row: " ++ show fields)
        fixtureRows = L.map parseFixtureLine fixtureLines
        actualDiff =
            [ title
            | (title, oldRule) <- fixtureRows
            , title /= RetainedEarnings
            , finalStockProbeRule title /= oldRule
            ]
    assertEqual "final-stock fixture covers all 116 concrete account titles"
        116 (L.length fixtureRows)
    assertEqual "final-stock registry closed diff = 27 formerly omitted accounts"
        finalStockExpectedClosedDiff actualDiff
    assertEqual "final-stock aggregate overrides remain open"
        ["Nothing", "Nothing", "Nothing", "Nothing", "Nothing"]
        [ finalStockProbeRule NetIncome
        , finalStockProbeRule NetLoss
        , finalStockProbeRule IncomeSummary
        , finalStockProbeRule NetIncomeAttributableToNCI
        , finalStockProbeRule NetLossAttributableToNCI
        ]

-- ================================================================
-- V-Land 2 scaffolding (語彙拡張の受理条件, レビュー非依存):
-- pre-vland2 fixture (tools/DumpVocabGolden.hs で生成, commit 85d6a7f に pin)
-- に対する Enum 挿入規律 pin と意味関数 closed-diff。
-- ================================================================

-- | V-Land 2 で既存意味が変わってよい科目の閉リスト。
-- scaffold 時点 (constructor 追加前) は空。外部レビュー裁定で既存科目の分類が
-- 変わる場合 (例: 有価証券 4 分類分解に伴う 'Securities' の再定義) は
-- ここに列挙して閉じる — 列挙外の意味変化は fail する。
vocabSemanticsExpectedClosedDiff :: [AccountTitles]
vocabSemanticsExpectedClosedDiff = []

-- | 挿入規律 pin: 語彙拡張は「既存 concrete constructor の Enum 序数を 1 つも
-- 動かさず, 新規は最大既存 concrete 序数と wildcard の間にのみ挿入し,
-- wildcard ('AccountTitle') は maxBound のまま」でなければならない
-- (Binary Word16 直列化互換と既存 fixture 世代の解釈可能性の要)。
testVocabOrdinalPin :: IO ()
testVocabOrdinalPin = do
    fixture <- TIO.readFile "test/fixtures/pre-vland2/ordinals.tsv"
    let parseOrd line = case T.splitOn (T.pack "\t") line of
            [name, ordText] -> (name, read (T.unpack ordText) :: Int)
            fields -> error ("invalid pre-vland2 ordinals row: " ++ show fields)
        rows =
            [ parseOrd line
            | line <- T.lines fixture
            , not (T.null line)
            , not (T.isPrefixOf (T.pack "#") line)
            ]
        current = M.fromList
            [ (T.pack (show t), fromEnum t)
            | t <- [minBound .. maxBound] :: [AccountTitles] ]
        wildcardName = T.pack (show (AccountTitle :: AccountTitles))
        pinnedConcrete = [ r | r@(n, _) <- rows, n /= wildcardName ]
        moved =
            [ (n, o, M.lookup n current)
            | (n, o) <- pinnedConcrete
            , M.lookup n current /= Just o ]
    assertEqual "vocab ordinal pin: fixture rows = 117 (116 concrete + wildcard)"
        117 (L.length rows)
    assertEqual "vocab ordinal pin: no pinned concrete ordinal moved" [] moved
    assertEqual "vocab ordinal pin: wildcard is maxBound"
        (fromEnum (maxBound :: AccountTitles))
        (fromEnum (AccountTitle :: AccountTitles))
    let maxPinned = L.maximum [ o | (_, o) <- pinnedConcrete ]
        pinnedNames = M.fromList [ (n, ()) | (n, _) <- rows ]
        misplaced =
            [ (n, o)
            | (n, o) <- M.toList current
            , not (M.member n pinnedNames)
            , not (o > maxPinned && o < fromEnum (maxBound :: AccountTitles)) ]
    assertEqual "vocab ordinal pin: new constructors sit between max pinned and wildcard"
        [] misplaced
    -- concreteAccountTitles は wildcard 以外の全 constructor を被覆すること。
    -- 現行の hardcoded 上限 ([Cash .. ReversalOfAllowanceForDoubtfulAccounts]) は
    -- 挿入後に新規科目が漏れるため, この assert が V-Land 2 に
    -- filter (/= wildcard) [minBound ..] への導出化を強制する。
    assertEqual "vocab ordinal pin: concreteAccountTitles covers all non-wildcard constructors"
        (L.filter (/= (AccountTitle :: AccountTitles)) [minBound .. maxBound])
        Registry.concreteAccountTitles

-- | 意味関数 closed-diff: 既存 116 科目の (division / isContra / whichSide
-- Not\/Hat / whatPIMO / fixedCurrent / finalStock probe) は,
-- 'vocabSemanticsExpectedClosedDiff' に列挙された科目を除き
-- pre-vland2 fixture と行単位で一致しなければならない。
testPreVland2SemanticsClosedDiff :: IO ()
testPreVland2SemanticsClosedDiff = do
    fixture <- TIO.readFile "test/fixtures/pre-vland2/semantics.tsv"
    let byName = M.fromList
            [ (T.pack (show t), t)
            | t <- [minBound .. maxBound] :: [AccountTitles] ]
        currentRow t =
            let nb = Not :< t :: HatBase AccountTitles
                hb = Hat :< t :: HatBase AccountTitles
            in T.intercalate (T.pack "\t")
                 [ T.pack (show t)
                 , T.pack (show (whatDiv nb))
                 , T.pack (show (Registry.classifyAccountContra t))
                 , T.pack (show (whichSide nb))
                 , T.pack (show (whichSide hb))
                 , T.pack (show (whatPIMO nb))
                 , T.pack (show (fixedCurrent nb))
                 , T.pack (finalStockProbeRule t)
                 ]
        rows =
            [ line
            | line <- T.lines fixture
            , not (T.null line)
            , not (T.isPrefixOf (T.pack "#") line) ]
        titleOf line = case T.splitOn (T.pack "\t") line of
            (name:_) -> case M.lookup name byName of
                Just t  -> t
                Nothing -> error ("pre-vland2 semantics: unknown title " ++ T.unpack name)
            [] -> error "pre-vland2 semantics: empty row"
        actualDiff =
            [ titleOf line | line <- rows, currentRow (titleOf line) /= line ]
    assertEqual "pre-vland2 semantics fixture covers all 116 concrete account titles"
        116 (L.length rows)
    assertEqual "pre-vland2 semantics closed diff"
        vocabSemanticsExpectedClosedDiff actualDiff

-- | R1 sentinel: a /balanced/ ledger (credit total == debit total, net income
-- zero) makes 'diffRL' report the wildcard 'Side'. Before the fix,
-- 'incomeSummaryAccount' matched only Credit/Debit and crashed with
-- "Non-exhaustive patterns". Run every closing-transfer function (Alg and
-- Journal) over a balanced ledger and force the result; none may throw.
--
-- The ledger pairs equal Sales (Revenue/Credit) and WageExpenditure
-- (Cost/Debit) amounts so @decR == decL@ (balanced).
balancedAlgSample :: TransferAlg
balancedAlgSample = EA.fromList
    [ 50 :@ Not :<(Sales,            1, 1, Yen)   -- credit (revenue)
    , 50 :@ Not :<(WageExpenditure,  1, 1, Yen)   -- debit  (cost)
    , 20 :@ Not :<(Purchases,        2, 2, Yen)   -- debit  (cost)
    , 20 :@ Not :<(InterestEarned,   2, 2, Yen)   -- credit (revenue)
    ]

balancedJournalSample :: TransferJournal
balancedJournalSample = EJ.fromList
    [ balancedAlgSample .| "A"
    , ((10 :@ Not :<(Sales, 3, 3, Yen)) .+ (10 :@ Not :<(Purchases, 3, 3, Yen))) .| "B"
    ]

testIncomeSummaryBalancedNoCrash :: IO ()
testIncomeSummaryBalancedNoCrash = do
    -- confirm the ledger really is balanced (triggers the wildcard Side)
    case EA.diffRL balancedAlgSample of
        (Side, _) -> return ()
        other     -> do putStrLn ("[FAIL] balanced sample not balanced: " ++ show (fst other))
                        exitFailure
    let algFns =
            [ ("incomeSummaryAccount",  EAT.incomeSummaryAccount)
            , ("netIncomeTransfer",     EAT.netIncomeTransfer)
            , ("grossProfitTransfer",   EAT.grossProfitTransfer)
            , ("ordinaryProfitTransfer",EAT.ordinaryProfitTransfer)
            , ("retainedEarningTransfer",EAT.retainedEarningTransfer)
            , ("finalStockTransfer",    EAT.finalStockTransfer)
            ]
        jFns =
            [ ("incomeSummaryAccount",  EJT.incomeSummaryAccount)
            , ("netIncomeTransfer",     EJT.netIncomeTransfer)
            , ("grossProfitTransfer",   EJT.grossProfitTransfer)
            , ("ordinaryProfitTransfer",EJT.ordinaryProfitTransfer)
            , ("retainedEarningTransfer",EJT.retainedEarningTransfer)
            , ("finalStockTransfer",    EJT.finalStockTransfer)
            ]
    forM_ algFns $ \(nm, f) -> do
        r <- try (evaluate (EA.norm (f balancedAlgSample)))
                :: IO (Either SomeException Double)
        case r of
            Right _ -> return ()
            Left e  -> do putStrLn ("[FAIL] Alg." ++ nm ++ " threw on balanced ledger: " ++ show e)
                          exitFailure
    forM_ jFns $ \(nm, f) -> do
        r <- try (evaluate (EA.norm (EJ.toAlg (f balancedJournalSample))))
                :: IO (Either SomeException Double)
        case r of
            Right _ -> return ()
            Left e  -> do putStrLn ("[FAIL] Journal." ++ nm ++ " threw on balanced ledger: " ++ show e)
                          exitFailure
    putStrLn "[PASS] all closing transfers identity-safe on balanced ledger (R1)"

type SpillRestoreJournal = EJ.Journal (String, Int) Double (HatBase CountUnit)

-- | Design-review C4: the spill/eviction decision logic is single-sourced in
-- 'ES.stepBackWith' / 'ES.spillDeleteDecision' (previously duplicated inline
-- in the classic spill loop and in Lite's retention loop). Pin the decision
-- table and the equivalence with Lite's former @backByTerms@.
testSpillDecisionSingleSource :: IO ()
testSpillDecisionSingleSource = do
    assertEqual "stepBackWith pred 3 10" (7 :: Int) (ES.stepBackWith pred 3 10)
    assertEqual "stepBackWith is id for n <= 0" (10 :: Int) (ES.stepBackWith pred 0 10)
    assertEqual "stepBackWith is id for negative n" (10 :: Int) (ES.stepBackWith pred (-1) 10)
    assertEqual "NoDelete evicts nothing"
        Nothing (ES.spillDeleteDecision pred (ES.NoDelete :: ES.SpillDeletePolicy Int) (1, 10))
    assertEqual "DeleteSpilledChunk evicts exactly the chunk"
        (Just (1, 10)) (ES.spillDeleteDecision pred ES.DeleteSpilledChunk (1 :: Int, 10))
    assertEqual "KeepRecentTerms 3 keeps the trailing window"
        (Just (1, 7)) (ES.spillDeleteDecision pred (ES.KeepRecentTerms 3) (1 :: Int, 10))
    assertEqual "KeepRecentTerms covering the chunk evicts nothing"
        Nothing (ES.spillDeleteDecision pred (ES.KeepRecentTerms 12) (1 :: Int, 10))
    -- Lite boundary equivalence: former backByTerms w t == stepBackWith pred w t
    let backByTermsRef w t = let go n x | n <= (0 :: Int) = x
                                        | otherwise       = go (n - 1) (pred x)
                             in go w t
    forM_ [(0, 5), (1, 5), (3, 5), (7, 5)] $ \(w, t) ->
        assertEqual ("Lite boundary equivalence w=" ++ show w)
            (backByTermsRef w t) (ES.stepBackWith pred w (t :: Int))

testRestoreJournalFromBinarySpill :: IO ()
testRestoreJournalFromBinarySpill = do
    let spillPath = "/tmp/exchangealgebra_spill_restore_test.bin"
        chunk1 :: SpillRestoreJournal
        chunk1 = EJ.fromList
            [ (1 :@ (Hat :< Yen)) .| ("A", 1)
            , (2 :@ (Not :< Amount)) .| ("B", 2)
            ]
        chunk2 :: SpillRestoreJournal
        chunk2 = (3 :@ (Hat :< Yen)) .| ("C", 3)
        currentLedger :: SpillRestoreJournal
        currentLedger = EJ.fromList
            [ (4 :@ (Not :< Amount)) .| ("Tail", 4)
            , (8 :@ (Hat :< Yen)) .| ("AlreadySpilled", 2)
            ]
        expected :: SpillRestoreJournal
        expected = chunk1 .+ chunk2 .+ ((4 :@ (Not :< Amount)) .| ("Tail", 4))

    withFile spillPath WriteMode $ \h -> do
        ES.defaultBinarySpillWriter h (1 :: Int, 2 :: Int) chunk1
        ES.defaultBinarySpillWriter h (3 :: Int, 3 :: Int) chunk2

    actual <- restoreJournalFromBinarySpill spillPath snd currentLedger
    assertEqual "Write.restoreJournalFromBinarySpill merges spill + tail remainder"
        (EJ.toMap expected)
        (EJ.toMap actual)

-- ================================================================
-- SimulateEx1 reproduction (default scenario only, no parallelism)
-- ================================================================

type SimTerm = Int

instance StateTime SimTerm where
    initTerm = 1
    lastTerm = 100
data SimInitVar = SimInitVar
    { _simInitStock        :: Double
    , _simSteadyProduction :: Double
    , _simInhouseRatio     :: Double
    } deriving (Eq, Show)

instance InitVariables SimInitVar where

data SimEvent
    = SimSalesPurchase
    | SimProduction
    | SimPlank
    deriving (Ord, Show, Enum, Eq, Bounded, Generic)

instance Hashable SimEvent where

instance Note SimEvent where
    plank = SimPlank

instance Event SimEvent where

type SimCompany = Int

instance Element SimCompany where
    wildcard = -1

instance BaseClass SimCompany where

simFstC, simLastC :: SimCompany
simFstC = 1
simLastC = 6

simCompanies :: [SimCompany]
simCompanies = [simFstC .. simLastC]

type SimHatBase2 = HatBase (AccountTitles, SimCompany, SimCompany, CountUnit)

instance ExBaseClass SimHatBase2 where
    getAccountTitle (h :< (a, _, _, _)) = a
    setAccountTitle (h :< (_, c, e, u)) b = h :< (b, c, e, u)

-- Accounting value type is MoneyDecimal (exact): ledger arithmetic is exact and
-- construction-order-independent. ABM parameters / input coefficients / random
-- draws remain Double and are converted (realToFrac) at the boundary where they
-- enter the ledger; reported stock/profit convert back to Double.
type SimTransaction = EJ.Journal (SimEvent, SimTerm) MoneyDecimal SimHatBase2

simCompressPreviousTerm :: SimTerm -> SimTransaction -> SimTransaction
simCompressPreviousTerm t le =
    EJ.fromMap $
        L.foldl' (\acc ev -> HM.adjust compress (ev, t) acc)
                 (EJ.toMap le)
                 [fstEvent .. lastEvent]

newtype SimLedger s = SimLedger (STRef s SimTransaction)

instance UpdatableSTRef SimLedger s SimTransaction where
    _unwrapURef (SimLedger x) = x
    _wrapURef x = SimLedger x

simInitLedger :: Double -> ST s (SimLedger s)
simInitLedger d = newURef $ EJ.fromList
    [ realToFrac d :@ Not :<(Products, e, e, Amount) .| (plank, initTerm)  -- Double param -> MoneyDecimal
    | e <- simCompanies
    ]

instance Updatable SimTerm SimInitVar SimLedger s where
    type Inner SimLedger s = STRef s SimTransaction
    unwrap = _unwrapURef
    initialize _ _ e = simInitLedger (_simInitStock e)
    updatePattern _ = return Modify
    modify _ t _ x = do
        le <- readURef x
        let added = EJ.gather (plank, t)
                  $ EJT.finalStockTransfer
                  $ (.-) $ simTermJournal (t - 1) le
            next = simCompressPreviousTerm (t - 1) (le .+ added)
        writeURef x next

type SimInputCoefficient = Double

newtype SimICTable s = SimICTable (STArray s (SimCompany, SimCompany) SimInputCoefficient)

instance UpdatableSTArray SimICTable s (SimCompany, SimCompany) SimInputCoefficient where
    _unwrapUArray (SimICTable arr) = arr
    _wrapUArray arr = SimICTable arr

simGenerateRandomList :: StdGen -> Int -> ([Double], StdGen)
simGenerateRandomList g n =
    let (xs, g') = runState (replicateM n (state (randomR (0, 1.0))))
                            (updateGen g 1000)
        ys = L.map (\v -> if v < 0.1 then 0 else v) xs
    in (ys, g')

simInitTermCoefficients :: StdGen -> Double -> M.Map SimCompany [SimInputCoefficient]
simInitTermCoefficients g inhouseRatio =
    fst $ L.foldl' buildRow (M.empty, g) simCompanies
  where
    buildRow (acc, g0) c2 =
        let (row, g1) = generateRow g0
        in (M.insert c2 row acc, g1)
    generateRow g0 =
        let (vals, g1) = simGenerateRandomList g0 simLastC
            total = sum vals
            normalized = L.map (\v -> (v / total) * inhouseRatio) vals
        in (normalized, g1)

simInitICTables :: StdGen -> Double -> ST s (SimICTable s)
simInitICTables g inhouseRatio = do
    arr <- newUArray ((simFstC, simFstC), (simLastC, simLastC)) 0
    let termCoefficients = simInitTermCoefficients g inhouseRatio
    forM_ simCompanies $ \c2 -> do
        let row = termCoefficients M.! c2
        forM_ (zip simCompanies row) $ \(c1, coef) ->
            writeUArray arr (c1, c2) coef
    return arr

instance Updatable SimTerm SimInitVar SimICTable s where
    type Inner SimICTable s = STArray s (SimCompany, SimCompany) SimInputCoefficient
    unwrap (SimICTable a) = a
    initialize g _ e = simInitICTables g (_simInhouseRatio e)
    updatePattern _ = return DoNothing

type SimSteadyProd = Double

newtype SimSP s = SimSP (STRef s SimSteadyProd)

instance UpdatableSTRef SimSP s SimSteadyProd where
    _unwrapURef (SimSP x) = x
    _wrapURef x = SimSP x

instance Updatable SimTerm SimInitVar SimSP s where
    type Inner SimSP s = STRef s SimSteadyProd
    unwrap = _unwrapURef
    initialize _ _ e = newURef (_simSteadyProduction e)
    updatePattern _ = return DoNothing

data SimWorld s = SimWorld
    { _simLedger :: SimLedger s
    , _simIcs    :: SimICTable s
    , _simSp     :: SimSP s
    } deriving (Generic)

-- helper functions

simTermJournal :: SimTerm -> SimTransaction -> SimTransaction
simTermJournal t = EJ.filterWithNote (\(_, t') _ -> t' == t)

simGetOneProduction :: SimWorld s -> SimTerm -> SimCompany -> ST s SimTransaction
simGetOneProduction wld t c = do
    let arr = _simIcs wld
    inputs <- mapM (\c2 -> do
        coef <- readUArray arr (c2, c)
        return $ realToFrac coef :@ Hat :<(Products, c2, c, Amount) .| (SimProduction, t)  -- Double coef -> MoneyDecimal
        ) simCompanies
    let totalInput = EJ.fromList inputs
        result = (1 :@ Not :<(Products, c, c, Amount) .| (SimProduction, t)) .+ totalInput
    return result

simJournal :: SimWorld s -> SimTransaction -> ST s ()
simJournal _ Zero = return ()
simJournal wld js = modifyURef (_simLedger wld) (\x -> x .+ js)

-- Values come from the MoneyDecimal ledger (via EA.toList), so the shortage map is
-- MoneyDecimal-valued; no conversion is needed and the amounts re-enter the ledger exactly.
simBuildShortageMap :: SimTerm -> SimTransaction -> M.Map (SimCompany, SimCompany) MoneyDecimal
simBuildShortageMap t le =
    let termAlg = EJ.toAlg $ (.-) $ simTermJournal t le
    in L.foldl' go M.empty (EA.toList termAlg)
  where
    go acc (v :@ (Hat :< (Products, j, i, Amount))) = M.insertWith (+) (i, j) v acc
    go acc _ = acc

simPurchases :: SimTerm -> SimWorld s -> ST s SimTransaction
simPurchases t wld = do
    le <- readURef (_simLedger wld)
    let shortageMap = simBuildShortageMap t le
        o i j = M.findWithDefault 0 (i, j) shortageMap
    return $ sigma simCompanies $ \i
           -> sigma (simCompanies L.\\ [i]) $ \j
           -> (o i j) :@ Not :<(Products, j, i, Amount)
           .+ (o i j) :@ Hat :<(Cash, (.#), i, Yen)
           .+ (o i j) :@ Not :<(Purchases, (.#), i, Yen)
           .+ (o i j) :@ Not :<(Cash, (.#), j, Yen)
           .+ (o i j) :@ Not :<(Sales, (.#), j, Yen)
           .+ (o i j) :@ Hat :<(Products, j, j, Amount)
           .| (SimSalesPurchase, t)

instance StateSpace SimTerm SimInitVar SimEvent SimWorld s where
    event = simEvent

simEvent :: SimWorld s -> SimTerm -> SimEvent -> ST s ()

simEvent wld t SimSalesPurchase = do
    toAdd <- simPurchases t wld
    simJournal wld toAdd

simEvent wld t SimProduction = do
    sp <- readURef (_simSp wld)
    forM_ simCompanies $ \e1 -> do
        op <- simGetOneProduction wld t e1
        simJournal wld (realToFrac sp .* op)  -- Double steady-production multiplier -> MoneyDecimal scalar

simEvent _ _ SimPlank = return ()

simGetTermStock :: SimWorld s -> SimTerm -> SimCompany -> ST s Double
simGetTermStock wld t e = do
    le <- readURef (_simLedger wld)
    let tj = (.-) $ simTermJournal t le
        plusStock  = norm $ EJ.projWithBase [Not :<(Products, e, e, Amount)] tj
        minusStock = norm $ EJ.projWithBase [Hat :<(Products, e, e, Amount)] tj
    return $ realToFrac (plusStock - minusStock)  -- exact MoneyDecimal stock -> Double for reporting

simGetTermGrossProfit :: SimWorld s -> SimTerm -> SimCompany -> ST s Double
simGetTermGrossProfit wld t e = do
    le <- readURef (_simLedger wld)
    let termTr = simTermJournal t le
        tr     = EJT.grossProfitTransfer termTr
        plus   = norm $ EJ.projWithBase [Not :<(GrossProfit, (.#), e, Yen)] tr
        minus  = norm $ EJ.projWithBase [Hat :<(GrossProfit, (.#), e, Yen)] tr
    return $ realToFrac (plus - minus)  -- exact MoneyDecimal -> Double for reporting

-- ================================================================
-- Simulation integration test
-- ================================================================

simEps :: Double
simEps = 1e-6

assertSimNear :: String -> Double -> Double -> IO ()
assertSimNear label expected actual
    | abs (expected - actual) <= simEps = putStrLn ("[PASS] " ++ label)
    | otherwise = do
        putStrLn ("[FAIL] " ++ label)
        putStrLn ("  expected: " ++ show expected)
        putStrLn ("  actual  : " ++ show actual)
        exitFailure

testSimulateEx1Default :: IO ()
testSimulateEx1Default = do
    let gen = mkStdGen 2025
        defaultEnv = SimInitVar
            { _simInitStock        = 20
            , _simInhouseRatio     = 0.4
            , _simSteadyProduction = 10
            }

    wld <- ES.runSimulation gen defaultEnv

    -- Stock at term 1 for each company
    stocks1 <- stToIO $ mapM (simGetTermStock wld 1) simCompanies
    -- Stock at term 50 for each company
    stocks50 <- stToIO $ mapM (simGetTermStock wld 50) simCompanies
    -- Stock at term 100 for each company
    stocks100 <- stToIO $ mapM (simGetTermStock wld 100) simCompanies
    -- Gross profit at term 50 for each company
    profits50 <- stToIO $ mapM (simGetTermGrossProfit wld 50) simCompanies

    -- Stock at t=1
    assertSimNear "sim1 stock(t=1,c=1)" 28.487224703666264 (stocks1 !! 0)
    assertSimNear "sim1 stock(t=1,c=3)" 30.0               (stocks1 !! 2)
    assertSimNear "sim1 stock(t=1,c=6)" 30.0               (stocks1 !! 5)  -- re-baselined: union zero-base fix removed a phantom self-input
    -- Stock at t=50
    assertSimNear "sim1 stock(t=50,c=1)" 304.9028131162567  (stocks50 !! 0)
    assertSimNear "sim1 stock(t=50,c=4)" 292.4764622201871  (stocks50 !! 3)
    -- Stock at t=100
    assertSimNear "sim1 stock(t=100,c=1)" 586.9595359862476  (stocks100 !! 0)
    assertSimNear "sim1 stock(t=100,c=6)" 767.9605634804993  (stocks100 !! 5)  -- re-baselined: union zero-base fix (bug compounded over terms)
    -- Gross profit at t=50
    assertSimNear "sim1 profit(t=50,c=1)" 0.35886554260018855 (profits50 !! 0)
    assertSimNear "sim1 profit(t=50,c=2)" 1.572544209772035   (profits50 !! 1)

-- ================================================================
-- CSV Write tests
-- ================================================================

testCsvTranspose :: IO ()
testCsvTranspose = do
    -- Square matrix
    let input1 = [ [T.pack "a", T.pack "b"]
                 , [T.pack "c", T.pack "d"] ]
        expected1 = [ [T.pack "a", T.pack "c"]
                    , [T.pack "b", T.pack "d"] ]
    assertEqual "CSV.transpose square matrix" expected1 (EW.csvTranspose input1)

    -- Ragged matrix (shorter rows padded with empty)
    let input2 = [ [T.pack "a", T.pack "b", T.pack "c"]
                 , [T.pack "d"] ]
        expected2 = [ [T.pack "a", T.pack "d"]
                    , [T.pack "b", T.empty]
                    , [T.pack "c", T.empty] ]
    assertEqual "CSV.transpose ragged matrix" expected2 (EW.csvTranspose input2)

    -- Single row
    let input3 = [[T.pack "x", T.pack "y", T.pack "z"]]
        expected3 = [[T.pack "x"], [T.pack "y"], [T.pack "z"]]
    assertEqual "CSV.transpose single row" expected3 (EW.csvTranspose input3)

    -- Empty
    assertEqual "CSV.transpose empty" ([] :: [[T.Text]]) (EW.csvTranspose [])

testCsvWriteCSV :: IO ()
testCsvWriteCSV = do
    let path = "/tmp/exchangealgebra_csv_test.csv"
        input = [ [T.pack "Name", T.pack "Value"]
                , [T.pack "Alice", T.pack "100"]
                , [T.pack "Bob", T.pack "200"] ]
    EW.writeCSV path input
    raw <- readFileStrict path
    -- Each cell should be quoted
    let lns = lines raw
    assertEqual "CSV writeCSV line count" 3 (length lns)
    assertEqual "CSV writeCSV header" "\"Name\",\"Value\"" (lns !! 0)
    assertEqual "CSV writeCSV row 1"  "\"Alice\",\"100\"" (lns !! 1)
    assertEqual "CSV writeCSV row 2"  "\"Bob\",\"200\""   (lns !! 2)
    removeFile path

testCsvWriteCSVWithQuotes :: IO ()
testCsvWriteCSVWithQuotes = do
    let path = "/tmp/exchangealgebra_csv_quote_test.csv"
        input = [[T.pack "say \"hello\"", T.pack "a,b"]]
    EW.writeCSV path input
    raw <- readFileStrict path
    let lns = lines raw
    -- Internal quotes should be escaped as ""
    assertEqual "CSV writeCSV escapes quotes" "\"say \"\"hello\"\"\",\"a,b\"" (lns !! 0)
    removeFile path

testCsvWriteCSVEmpty :: IO ()
testCsvWriteCSVEmpty = do
    let path = "/tmp/exchangealgebra_csv_empty_test.csv"
        input = [[T.pack "", T.pack "x"]]
    EW.writeCSV path input
    raw <- readFileStrict path
    let lns = lines raw
    assertEqual "CSV writeCSV empty cell" "\"\",\"x\"" (lns !! 0)
    removeFile path

-- ================================================================
-- Legacy-generation writer output-pinning tests (design-review C7)
--
-- These pin the exact CSV bytes produced, for small fixed inputs, by the
-- "legacy generation" writers (writeBS / writePL / writeJournal /
-- writeCompoundTrialBalance / writeAccountOfJournal) as computed by the
-- pre-refactor implementation. Their purpose is to let the "generation
-- unification" refactor (pure *Rows builder + thin IO wrapper, matching the
-- worksheetRows/postClosingTrialBalanceRows/accountLedgerRows style) be
-- verified to leave output bit-for-bit unchanged: these must stay green,
-- unmodified, across the refactor.
-- ================================================================

testWriteBSPinned :: IO ()
testWriteBSPinned = do
    let path = "/tmp/exchangealgebra_write_bs_pinned_test.csv"
        alg = (100 .@ Not :< Cash)
            .+ (60  .@ Not :< LoansPayable)
            .+ (40  .@ Not :< CapitalStock)
            :: EA.Alg Double (HatBase AccountTitles)
    EW.writeBS path alg
    raw <- readFileStrict path
    removeFile path
    let lns = lines raw
    assertEqual "writeBS pinned: line count" 5 (length lns)
    assertEqual "writeBS pinned: row0 (Asset/Liability headers)"
        "\"Asset\",\"\",\"Liability\",\"\"" (lns !! 0)
    assertEqual "writeBS pinned: row1 (Cash/LoansPayable)"
        "\"Cash\",\"100.0\",\"LoansPayable\",\"60.0\"" (lns !! 1)
    assertEqual "writeBS pinned: row2 (Total/Equity header)"
        "\"Total\",\"100.0\",\"Equity\",\"\"" (lns !! 2)
    assertEqual "writeBS pinned: row3 (CapitalStock)"
        "\"\",\"\",\"CapitalStock\",\"40.0\"" (lns !! 3)
    assertEqual "writeBS pinned: row4 (grand total)"
        "\"\",\"\",\"Total\",\"100.0\"" (lns !! 4)

testWritePLPinned :: IO ()
testWritePLPinned = do
    let path = "/tmp/exchangealgebra_write_pl_pinned_test.csv"
        alg = (500 .@ Not :< Sales)
            .+ (300 .@ Not :< SalesCost)
            :: EA.Alg Double (HatBase AccountTitles)
    EW.writePL path alg
    raw <- readFileStrict path
    removeFile path
    let lns = lines raw
    assertEqual "writePL pinned: line count" 3 (length lns)
    assertEqual "writePL pinned: row0 (Cost/Revenue headers)"
        "\"Cost\",\"\",\"Revenue\",\"\"" (lns !! 0)
    assertEqual "writePL pinned: row1 (SalesCost/Sales)"
        "\"SalesCost\",\"300.0\",\"Sales\",\"500.0\"" (lns !! 1)
    assertEqual "writePL pinned: row2 (totals)"
        "\"Total\",\"500.0\",\"Total\",\"300.0\"" (lns !! 2)

testWriteJournalPinned :: IO ()
testWriteJournalPinned = do
    let path = "/tmp/exchangealgebra_write_journal_pinned_test.csv"
        d1 = fromGregorian 2024 4 1
        d2 = fromGregorian 2024 4 2
        d3 = fromGregorian 2024 4 3
        getDay' :: HatBase (AccountTitles, Day) -> Day
        getDay' (_ :< (_, d)) = d
        alg = (100 .@ Not :< (Cash, d1))
            .+ (100 .@ Not :< (CapitalStock, d1))
            .+ (50  .@ Not :< (Cash, d2))
            .+ (50  .@ Not :< (Sales, d2))
            .+ (30  .@ Not :< (Cash, d3))
            .+ (10  .@ Not :< (AccountsReceivable, d3))
            .+ (40  .@ Not :< (Sales, d3))
            :: EA.Alg Double (HatBase (AccountTitles, Day))
    EW.writeJournal path alg getDay'
    raw <- readFileStrict path
    removeFile path
    let lns = lines raw
    assertEqual "writeJournal pinned: line count" 5 (length lns)
    assertEqual "writeJournal pinned: header"
        "\"Day\",\"Debit\",\"Amount\",\"Credit\",\"Amount\"" (lns !! 0)
    assertEqual "writeJournal pinned: day1"
        "\"2024-04-01\",\"Cash\",\"100.0\",\"CapitalStock\",\"100.0\"" (lns !! 1)
    assertEqual "writeJournal pinned: day2"
        "\"2024-04-02\",\"Cash\",\"50.0\",\"Sales\",\"50.0\"" (lns !! 2)
    assertEqual "writeJournal pinned: day3 line1 (2 debits vs 1 credit -> toSameLength padding)"
        "\"2024-04-03\",\"AccountsReceivable\",\"10.0\",\"Sales\",\"40.0\"" (lns !! 3)
    assertEqual "writeJournal pinned: day3 line2 (padded Day/Credit cells empty)"
        "\"\",\"Cash\",\"30.0\",\"\",\"\"" (lns !! 4)

testWriteCompoundTrialBalancePinned :: IO ()
testWriteCompoundTrialBalancePinned = do
    let path = "/tmp/exchangealgebra_write_ctb_pinned_test.csv"
        alg = (100 .@ Not :< Cash)
            .+ (60  .@ Not :< LoansPayable)
            .+ (40  .@ Not :< CapitalStock)
            :: EA.Alg Double (HatBase AccountTitles)
    EW.writeCompoundTrialBalance path alg
    raw <- readFileStrict path
    removeFile path
    let lns = lines raw
    assertEqual "writeCompoundTrialBalance pinned: line count" 5 (length lns)
    assertEqual "writeCompoundTrialBalance pinned: header"
        "\"Debit Balance\",\"Debit Total\",\"Account Title\",\"Credit Total\",\"Credit Balance\""
        (lns !! 0)
    -- NOTE (legacy layout quirk, preserved verbatim): a debit-balance account
    -- (Cash: gross debit 100 / credit 0) places its balance figure in the
    -- *Credit Balance* column (rightmost), not the *Debit Balance* column,
    -- and a credit-balance account (CapitalStock/LoansPayable) places it in
    -- *Debit Balance* (leftmost) -- the opposite of the (side,mag) ->
    -- (debitCell,creditCell) convention 'sideCells' uses elsewhere
    -- (worksheetRows / postClosingTrialBalanceRows). See the Haddock on
    -- 'compoundTrialBalanceRows' for why this was kept as explicit case
    -- analysis instead of being consolidated onto 'sideCells'.
    assertEqual "writeCompoundTrialBalance pinned: Cash (debit-heavy -> Credit Balance col)"
        "\"\",\"100.0\",\"Cash\",\"0.0\",\"100.0\"" (lns !! 1)
    assertEqual "writeCompoundTrialBalance pinned: CapitalStock (credit-heavy -> Debit Balance col)"
        "\"40.0\",\"0.0\",\"CapitalStock\",\"40.0\",\"\"" (lns !! 2)
    assertEqual "writeCompoundTrialBalance pinned: LoansPayable (credit-heavy -> Debit Balance col)"
        "\"60.0\",\"0.0\",\"LoansPayable\",\"60.0\",\"\"" (lns !! 3)
    assertEqual "writeCompoundTrialBalance pinned: totals"
        "\"100.0\",\"100.0\",\"Total\",\"100.0\",\"100.0\"" (lns !! 4)

testWriteAccountOfJournalPinned :: IO ()
testWriteAccountOfJournalPinned = do
    let path = "/tmp/exchangealgebra_write_aoj_pinned_test.csv"
        jrn = ((100 .@ Not :< Cash) .| "sale")
           .+ ((40  .@ Hat :< Cash) .| "pay")
            :: Journal String Double (HatBase AccountTitles)
    EW.writeAccountOfJournal [Cash] path jrn
    raw <- readFileStrict path
    removeFile path
    let lns = lines raw
    assertEqual "writeAccountOfJournal pinned: line count" 4 (length lns)
    assertEqual "writeAccountOfJournal pinned: title header"
        "\"Cash\",\"\",\"\"" (lns !! 0)
    assertEqual "writeAccountOfJournal pinned: sub header"
        "\"Note\",\"Debit\",\"Credit\"" (lns !! 1)
    assertEqual "writeAccountOfJournal pinned: note order (\"pay\" < \"sale\")"
        "\"\"\"pay\"\"\",\"\",\"40.0\"" (lns !! 2)
    assertEqual "writeAccountOfJournal pinned: sale posting"
        "\"\"\"sale\"\"\",\"100.0\",\"\"" (lns !! 3)

-- | Regression tests for scale-aware numeric tolerance (WI-11/12/14).
-- These exercise large magnitudes that the previous fixed @1e-13@ absolute
-- tolerance handled incorrectly (retaining pure rounding noise as a residual);
-- small-scale behavior is unchanged. See plans LAZY_EVAL_AUDIT.md s4.6.
testNumericToleranceScaleAware :: IO ()
testNumericToleranceScaleAware = do
    assertEqual "nearlyEqScaled: large-scale rounding treated as equal"
        True  (EA.nearlyEqScaled (1e10 + 0.1 + 0.2) (1e10 + 0.3 :: Double))
    assertEqual "isNearlyNum 1e-13: large-scale rounding rejected (documents old flaw)"
        False (EA.isNearlyNum (1e10 + 0.1 + 0.2) (1e10 + 0.3) (1e-13 :: Double))
    assertEqual "nearlyEqScaled: small-scale noise treated as equal"
        True  (EA.nearlyEqScaled (0.1 + 0.2) (0.3 :: Double))
    assertEqual "nearlyEqScaled: genuine residual kept (not swallowed)"
        False (EA.nearlyEqScaled (1e10 + 5.0) (1e10 :: Double))
    assertEqual "nearlyEqScaled: NaN guarded (no crash, not equal)"
        False (EA.nearlyEqScaled (0/0) (1.0 :: Double))
    let big = (1e10 :@ (Hat :< Yen)) .+ (0.1 :@ (Hat :< Yen)) .+ (0.2 :@ (Hat :< Yen))
           .+ (1e10 :@ (Not :< Yen)) .+ (0.3 :@ (Not :< Yen)) :: TestAlg
    assertEqual "bar cancels balanced large-scale element to Zero"
        True (EA.isZero ((.-) big))

-- | Smoke test for the exact non-negative decimal value type 'MoneyDecimal' (Stage B).
-- The point of an exact value type is that summation is associative, so @norm@ is
-- *independent of construction order* — the property that makes the fromList O(N)
-- optimization safe (Stage D). Note the raw @Seq@ order (and hence @toMap@/@Eq@)
-- still depends on construction; only the numeric results are order-independent.
testMoneyDecimalExactOrderIndependent :: IO ()
testMoneyDecimalExactOrderIndependent = do
    assertEqual "MoneyDecimal: 0.1 + 0.2 == 0.3 exactly"
        True (0.1 + 0.2 == (0.3 :: MoneyDecimal))
    let mk i = ((fromIntegral (i `mod` 7 + 1) :: MoneyDecimal)
                  :@ ((if even i then Hat else Not) :< ([Yen, Amount] !! (i `mod` 2))))
               .| show (i `mod` 150)
        xs       :: [Journal String MoneyDecimal (HatBase CountUnit)]
        xs       = [ mk i | i <- [1 .. 400 :: Int] ]
        viaFoldr = foldr (.+) mempty xs
        viaFoldl = L.foldl' (.+) mempty xs
    -- exact ⇒ norm is identical for the two construction orders
    assertEqual "MoneyDecimal Journal: norm is construction-order-independent"
        (norm viaFoldr) (norm viaFoldl)
    -- banker's rounding (round half to even)
    assertEqual "bankersRound 0 2.5 = 2 (half to even)" (2 :: MoneyDecimal) (bankersRound 0 2.5)
    assertEqual "bankersRound 0 3.5 = 4 (half to even)" (4 :: MoneyDecimal) (bankersRound 0 3.5)
    assertEqual "bankersRound 2 0.125 = 0.12 (half to even)" (0.12 :: MoneyDecimal) (bankersRound 2 0.125)

-- | Strict file read helper for tests
readFileStrict :: FilePath -> IO String
readFileStrict p = do
    bs <- TIO.readFile p
    return (T.unpack bs)

-- ================================================================
-- Main
-- ================================================================

-- ================================================================
-- Redundant-algebra axiom property tests (QuickCheck)
--
-- Encodes the Definition 6 axioms (paper Appendix A) + derived lemmas as
-- QuickCheck properties, plus regression generalizations for the union
-- zero-base bug and construction-order independence. Property suite, additive.
-- ================================================================

type NNAlg = EA.Alg MoneyDecimal (HatBase CountUnit)

-- run a QuickCheck property in the existing IO-style harness
quickProp :: Testable p => String -> p -> IO ()
quickProp label p = do
    r <- quickCheckWithResult stdArgs { maxSuccess = 200, chatty = False } p
    if isSuccess r
        then putStrLn ("[PASS] " ++ label)
        else do putStrLn ("[FAIL] " ++ label); putStr (output r); exitFailure

-- generators: concrete (non-wildcard) bases, intentional collisions
genUnit :: Gen CountUnit
genUnit = elements [Yen, Dollar, Amount]

genSide :: Gen Hat
genSide = elements [Hat, Not]

genBase :: Gen (HatBase CountUnit)
genBase = (:<) <$> genSide <*> genUnit

genNNDouble :: Gen Double          -- non-negative, finite
genNNDouble = do
    NonNegative x <- arbitrary
    if isNaN x || isInfinite x then genNNDouble else pure x

genAlgD :: Gen TestAlg
genAlgD = sized $ \n -> do
    k  <- choose (0, min 40 n)
    ps <- vectorOf k ((,) <$> genNNDouble <*> genBase)
    pure (EA.fromList [ v .@ b | (v, b) <- ps ])

genAlgN :: Gen NNAlg
genAlgN = sized $ \n -> do
    k  <- choose (0, min 40 n)
    ps <- vectorOf k ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)
    pure (EA.fromList [ v .@ b | (v, b) <- ps ])

-- exact per-base signed net (Not +, Hat -) via Rational; the observable
-- accounting content. Robust to seq order; catches base misassociation.
netByBase :: (HatVal v, Real v) => EA.Alg v (HatBase CountUnit) -> M.Map CountUnit Rational
netByBase = EA.foldEntries step M.empty
  where
    step m v b = M.insertWith (+) (part b) (signed v b) m
    part (_ :< u) = u
    signed v b = if isHat b then negate (toRational v) else toRational v

epsEq :: Double -> Double -> Bool
epsEq a b = abs (a - b) <= 1e-9 * (1 + max (abs a) (abs b))

-- ================================================================
-- ExchangeAlgebra.Convert.Csv: generic journal CSV reader.
-- Read-only round-trip property: a generated list of postings rendered to a
-- fixed-schema CSV string parses back to exactly the term built directly by
-- journalFromSides (MoneyDecimal = exact, so strict equality, no tolerance).
-- ================================================================

-- concrete account titles only (no wildcard); use canonical Show names so the
-- CSV round-trip does not exercise the ambiguous-alias path.
genAccountTitle :: Gen AccountTitles
genAccountTitle = elements EC.concreteAccountTitles

genSideCsv :: Gen Side
genSideCsv = elements [Debit, Credit]

-- non-negative MoneyDecimal with up to 2 decimal places, written exactly as a
-- decimal literal (terminating) so scientificAmount parses it back exactly.
genAmountMD :: Gen (MoneyDecimal, T.Text)
genAmountMD = do
    whole  <- choose (0, 99999) :: Gen Integer
    cents  <- choose (0, 99)    :: Gen Integer
    let txt = T.pack (show whole) <> T.pack "." <>
              T.pack (let s = show cents in if length s == 1 then '0':s else s)
        val = fromRational (toRational whole + toRational cents / 100) :: MoneyDecimal
    pure (val, txt)

genPostingCsv :: Gen (Side, AccountTitles, MoneyDecimal, T.Text)
genPostingCsv = do
    s        <- genSideCsv
    a        <- genAccountTitle
    (v, vtx) <- genAmountMD
    pure (s, a, v, vtx)

renderCsv :: [(Side, AccountTitles, MoneyDecimal, T.Text)] -> T.Text
renderCsv rows =
    T.unlines (header : L.map line rows)
  where
    header = T.pack "side,account,amount"
    line (s, a, _, vtx) =
        T.intercalate (T.pack ",")
            [ sideText s, T.pack (show a), vtx ]
    sideText Debit  = T.pack "debit"
    sideText Credit = T.pack "credit"
    sideText Side   = T.pack "debit"   -- unused (generator never yields wildcard)

testConvertCsvRoundTrip :: IO ()
testConvertCsvRoundTrip = do
    quickProp "convert-csv: render -> parse is exact (MoneyDecimal)" $
        forAll (resize 30 (listOf genPostingCsv)) $ \rows ->
            let csv      = renderCsv rows
                expected = EC.journalFromSides
                             [ (s, a, v) | (s, a, v, _) <- rows ]
                           :: EA.Alg MoneyDecimal (HatBase AccountTitles)
                parsed   = ECsv.parseJournalCsv csv
                           :: Either EC.ConvError
                                     (EA.Alg MoneyDecimal (HatBase AccountTitles))
            in parsed == Right expected

    -- structural guards: bad header, unknown account, negative amount, bad arity.
    let badHeader = T.pack "s,a,amt\ndebit,Cash,1\n"
        badAcct   = T.pack "side,account,amount\ndebit,Goodwill_X,1\n"
        badAmt    = T.pack "side,account,amount\ndebit,Cash,-1\n"
        badArity  = T.pack "side,account,amount\ndebit,Cash\n"
        run t = ECsv.parseJournalCsv t
                  :: Either EC.ConvError
                            (EA.Alg MoneyDecimal (HatBase AccountTitles))
        expectLeft label pat t = case run t of
            Left e | pat e     -> putStrLn ("[PASS] " ++ label)
                   | otherwise -> do putStrLn ("[FAIL] " ++ label ++ ": wrong error " ++ show e); exitFailure
            Right _            -> do putStrLn ("[FAIL] " ++ label ++ ": accepted bad input"); exitFailure
    expectLeft "convert-csv: rejects bad header"
        (\e -> case e of EC.MalformedCsv _ -> True; _ -> False) badHeader
    expectLeft "convert-csv: rejects unknown account"
        (\e -> case e of EC.UnknownAccount _ -> True; _ -> False) badAcct
    expectLeft "convert-csv: rejects negative amount"
        (\e -> case e of EC.BadAmount _ -> True; _ -> False) badAmt
    expectLeft "convert-csv: rejects wrong field count"
        (\e -> case e of EC.MalformedCsv _ -> True; _ -> False) badArity

-- ================================================================
-- ExchangeAlgebra.Assist: account descriptions and LLM feedback helpers.
-- ================================================================

testAssistDescriptionsDrift :: IO ()
testAssistDescriptionsDrift =
    assertEqual "Assist descriptions are the registry projection"
        registryProjection AssistDesc.accountDescriptions
  where
    registryProjection =
        [ (title, Registry.asNameEn spec, Registry.asNameJa spec, Registry.asDescription spec)
        | title <- Registry.concreteAccountTitles
        , Just spec <- [Registry.accountSpec title]
        ]

testAssistDescribeAccount :: IO ()
testAssistDescribeAccount = do
    let missing =
            [ title
            | title <- EC.concreteAccountTitles
            , Assist.describeAccount title == Nothing
            ]
    assertEqual "Assist.describeAccount covers every concrete account"
        ([] :: [AccountTitles]) missing
    assertEqual "Assist.describeAccount rejects wildcard AccountTitle"
        Nothing (Assist.describeAccount AccountTitle)

testAssistAllAccountInfos :: IO ()
testAssistAllAccountInfos = do
    assertEqual "Assist.allAccountInfos length" 232 (length Assist.allAccountInfos)
    assertEqual "Assist.allAccountInfos follows concreteAccountTitles order"
        EC.concreteAccountTitles (L.map Assist.aiTitle Assist.allAccountInfos)
    forM_ Assist.allAccountInfos $ \info -> do
        let title = Assist.aiTitle info
        case Registry.accountSemantics title of
            Nothing -> do
                putStrLn ("[FAIL] missing account semantics: " ++ show title)
                exitFailure
            Just semantics -> do
                assertEqual ("Assist.aiRoles " ++ show title)
                    (Registry.asemRoles semantics) (Assist.aiRoles info)
                assertEqual ("Assist.aiPostingCapability " ++ show title)
                    (Registry.asemPostingCapability semantics)
                    (Assist.aiPostingCapability info)
                assertEqual ("Assist.aiDivisionSemantics " ++ show title)
                    (Registry.asemDivisionSemantics semantics)
                    (Assist.aiDivisionSemantics info)
                assertEqual ("Assist.aiHomeSideSemantics " ++ show title)
                    (Registry.asemHomeSideSemantics semantics)
                    (Assist.aiHomeSideSemantics info)
                assertEqual ("Assist.aiReportingEligibility " ++ show title)
                    (Registry.asemReportingEligibility semantics)
                    (Assist.aiReportingEligibility info)

testAccountMetadataLand1 :: IO ()
testAccountMetadataLand1 = do
    let semantics =
            [ (title, value)
            | title <- Registry.concreteAccountTitles
            , Just value <- [Registry.accountSemantics title]
            ]
        exceptional =
            [ NetIncome, NetLoss, GrossProfit, OrdinaryProfit, IncomeSummary
            , SuspensePayments, SuspenseReceipts, CashOverShort, SuspenseAccount
            , BranchCurrentAccount, HeadOfficeCurrentAccount
            , NetIncomeAttributableToNCI, NetLossAttributableToNCI
            ]
        lookupSem title = Registry.accountSemantics title
        lookupInfo title = Assist.describeAccount title
        nonStatementTitles =
            [ title
            | (title, value) <- semantics
            , case Registry.asemDivisionSemantics value of
                StatementDivision _ -> False
                _                   -> True
            ]
    assertEqual "Land 1 metadata covers all 232 concrete titles"
        232 (L.length semantics)
    assertEqual "Land 1 metadata rejects wildcard AccountTitle"
        Nothing (Registry.accountSemantics AccountTitle)
    assertEqual "Land 1 non-statement metadata is exactly the reviewed exception set"
        (L.sort exceptional) (L.sort nonStatementTitles)
    forM_ semantics $ \(title, value) -> do
        assertEqual ("Land 1 roles are non-empty: " ++ show title)
            True (not (L.null (Registry.asemRoles value)))
        case Registry.asemDivisionSemantics value of
            StatementDivision division -> do
                assertEqual ("Land 1 statement division preserves legacy value: " ++ show title)
                    (classifyAccountDivision title) division
            _ -> assertEqual ("Land 1 exceptional title is closed-listed: " ++ show title)
                    True (title `L.elem` exceptional)
        case Registry.asemHomeSideSemantics value of
            FixedHomeSide side ->
                assertEqual ("Land 1 fixed home side preserves legacy value: " ++ show title)
                    (whichSide (Not :< title)) side
            _ -> pure ()
    assertEqual "Land 1 Cash semantics"
        (Just ( [OrdinaryAccount], OrdinaryPosting
              , StatementDivision Assets, FixedHomeSide Debit, StatementEligible ))
        (fmap semanticsTuple (lookupSem Cash))
    assertEqual "Land 1 IncomeSummary semantics"
        (Just ( [ClosingDevice], ClosingOnly
              , DirectionEncoding Assets, ContextDependentHomeSide, NotPresented ))
        (fmap semanticsTuple (lookupSem IncomeSummary))
    assertEqual "Land 1 NetIncome semantics"
        (Just ( [PeriodResult], EngineGeneratedOnly
              , DirectionEncoding Cost, FixedHomeSide Debit, DerivedPresentation ))
        (fmap semanticsTuple (lookupSem NetIncome))
    assertEqual "Land 1 GrossProfit is an engine-generated coordinate"
        (Just ( [ReportingSubtotal], EngineGeneratedOnly
              , DirectionEncoding Revenue, FixedHomeSide Credit
              , DerivedPresentation ))
        (fmap semanticsTuple (lookupSem GrossProfit))
    assertEqual "Land 1 NCI profit is distinct from bare net income"
        (Just ( [AttributionAccount, PeriodResult], ConsolidationOnly
              , DirectionEncoding Cost, FixedHomeSide Debit
              , ContextualPresentation ))
        (fmap semanticsTuple (lookupSem NetIncomeAttributableToNCI))
    assertEqual "Land 1a NCI equity is consolidation-only"
        (Just ( [AttributionAccount], ConsolidationOnly
              , StatementDivision Equity, FixedHomeSide Credit
              , ContextualPresentation ))
        (fmap semanticsTuple (lookupSem NonControllingInterests))
    assertEqual "Land 1 branch account semantics"
        (Just ( [ReciprocalAccount], OrdinaryPosting
              , BookkeepingControlClass Assets, FixedHomeSide Debit
              , ContextualPresentation ))
        (fmap semanticsTuple (lookupSem BranchCurrentAccount))
    assertEqual "Land 1 IncomeSummary LLM description does not classify it as an asset"
        True (case lookupInfo IncomeSummary of
            Just info -> not (T.isPrefixOf (T.pack "Asset") (Assist.aiDesc info))
                      && T.isInfixOf (T.pack "not a balance-sheet classification")
                                     (Assist.aiDesc info)
            Nothing -> False)
    assertEqual "Land 1 NetIncome LLM name drops legacy Expense wording"
        (Just (T.pack "当期純利益")) (fmap Assist.aiNameJa (lookupInfo NetIncome))
  where
    semanticsTuple value =
        ( Registry.asemRoles value
        , Registry.asemPostingCapability value
        , Registry.asemDivisionSemantics value
        , Registry.asemHomeSideSemantics value
        , Registry.asemReportingEligibility value
        )

accountMetadataLand1Header :: T.Text -> T.Text
accountMetadataLand1Header what =
    T.pack "# account-semantics-050 Land 1 " <> what
    <> T.pack "; schema 1; base 09c8a60c0bfb1a7fedb01689ceee789b8b4e6084\n"

accountMetadataLand1Row :: AccountTitles -> T.Text
accountMetadataLand1Row title = case Registry.accountSemantics title of
    Nothing -> error ("missing AccountSemantics for " ++ show title)
    Just semantics -> T.intercalate (T.pack "\t")
        [ goldenShow title
        , goldenShow (Registry.asemRoles semantics)
        , goldenShow (Registry.asemPostingCapability semantics)
        , goldenShow (Registry.asemDivisionSemantics semantics)
        , goldenShow (Registry.asemHomeSideSemantics semantics)
        , goldenShow (Registry.asemReportingEligibility semantics)
        ]

accountMetadataLand1InfoRow :: Assist.AccountInfo -> T.Text
accountMetadataLand1InfoRow info = T.intercalate (T.pack "\t")
    [ goldenShow (Assist.aiTitle info)
    , goldenShow (Assist.aiRoles info)
    , goldenShow (Assist.aiPostingCapability info)
    , goldenShow (Assist.aiDivisionSemantics info)
    , goldenShow (Assist.aiHomeSideSemantics info)
    , goldenShow (Assist.aiReportingEligibility info)
    , goldenEsc (Assist.aiNameEn info)
    , goldenEsc (Assist.aiNameJa info)
    , goldenEsc (Assist.aiDesc info)
    ]

accountMetadataLand1Suggestions :: T.Text
accountMetadataLand1Suggestions =
    accountMetadataLand1Header
        (T.pack "LLM suggestAccounts (query, total matches, top-10 titles)")
    <> T.unlines (L.map row corpus)
  where
    infos = Assist.allAccountInfos
    fields = L.concat
        [ [goldenShow (Assist.aiTitle info), Assist.aiNameEn info, Assist.aiNameJa info]
        | info <- infos
        ]
    descTokens = L.concatMap (T.words . Assist.aiDesc) infos
    corpus = goldenDedupSort
        (L.concatMap (\value -> [value, T.toLower value]) fields <> descTokens)
    row query =
        let matches = L.map Assist.aiTitle (Assist.suggestAccounts query)
        in goldenEsc query <> T.pack "\t" <> goldenShow (L.length matches)
           <> T.pack "\t"
           <> T.intercalate (T.pack ",") (L.map goldenShow (L.take 10 matches))

testAccountMetadataLand1Golden :: IO ()
testAccountMetadataLand1Golden = do
    metadata <- TIO.readFile "test/fixtures/account-semantics-050/metadata.tsv"
    info <- TIO.readFile "test/fixtures/account-semantics-050/account-info.tsv"
    suggest <- TIO.readFile "test/fixtures/account-semantics-050/suggest.tsv"
    let expectedMetadata =
            accountMetadataLand1Header
                (T.pack "registry (title, roles, posting, divisionSemantics, homeSideSemantics, reportingEligibility)")
            <> T.unlines (L.map accountMetadataLand1Row Registry.concreteAccountTitles)
        expectedInfo =
            accountMetadataLand1Header
                (T.pack "LLM AccountInfo (title, roles, posting, divisionSemantics, homeSideSemantics, reportingEligibility, nameEn, nameJa, description)")
            <> T.unlines (L.map accountMetadataLand1InfoRow Assist.allAccountInfos)
    assertEqual "Land 1 metadata fixture has 232 rows"
        232 (L.length (L.drop 1 (T.lines metadata)))
    assertEqual "Land 1 metadata fixture" metadata expectedMetadata
    assertEqual "Land 1 LLM AccountInfo fixture" info expectedInfo
    assertEqual "Land 1 LLM suggestion fixture"
        suggest accountMetadataLand1Suggestions

testAccountInfoLand1Migration :: IO ()
testAccountInfoLand1Migration = do
    legacy <- TIO.readFile "test/fixtures/pre-account-semantics-050/account-info.tsv"
    let titleMap = M.fromList
            [ (goldenShow title, title) | title <- Registry.concreteAccountTitles ]
        rows = L.filter (not . T.null) (L.drop 1 (T.lines legacy))
    assertEqual "Land 1 AccountInfo migration covers 232 legacy rows"
        232 (L.length rows)
    forM_ rows $ \line -> case T.splitOn (T.pack "\t") line of
        [titleText, oldDivision, oldSide, oldNameEn, oldNameJa, oldDesc] ->
            case M.lookup titleText titleMap of
                Nothing -> assertEqual "Land 1 migration unknown legacy title"
                    (T.pack "") titleText
                Just title -> case (Registry.accountSemantics title, Assist.describeAccount title) of
                    (Just semantics, Just info) -> do
                        assertEqual ("Land 1 legacy division is recoverable: " ++ show title)
                            oldDivision
                            (legacyDivisionText (Registry.asemDivisionSemantics semantics))
                        assertEqual ("Land 1 legacy home side is recoverable: " ++ show title)
                            oldSide (goldenShow (whichSide (Not :< title)))
                        case Registry.asemDivisionSemantics semantics of
                            StatementDivision _ -> do
                                assertEqual ("Land 1 ordinary nameEn unchanged: " ++ show title)
                                    oldNameEn (goldenEsc (Assist.aiNameEn info))
                                assertEqual ("Land 1 ordinary nameJa unchanged: " ++ show title)
                                    oldNameJa (goldenEsc (Assist.aiNameJa info))
                                assertEqual ("Land 1 ordinary description unchanged: " ++ show title)
                                    oldDesc (goldenEsc (Assist.aiDesc info))
                            _ -> pure ()
                    _ -> do
                        putStrLn ("[FAIL] missing Land 1 migration metadata: " ++ show title)
                        exitFailure
        _ -> assertEqual "Land 1 migration malformed legacy row" (T.pack "") line
  where
    legacyDivisionText semantics = goldenShow $ case semantics of
        StatementDivision division        -> division
        BookkeepingControlClass division  -> division
        DirectionEncoding division        -> division
        NoStatementDivision -> error "legacy division is unavailable"

testAssistSuggestAccounts :: IO ()
testAssistSuggestAccounts = do
    assertEqual "Assist.suggestAccounts cash contains Cash"
        True (Cash `elem` L.map Assist.aiTitle (Assist.suggestAccounts (T.pack "cash")))
    assertEqual "Assist.suggestAccounts 現金 contains Cash"
        True (Cash `elem` L.map Assist.aiTitle (Assist.suggestAccounts (T.pack "現金")))
    assertEqual "Assist.suggestAccounts empty query"
        [] (Assist.suggestAccounts T.empty)
    assertEqual "Assist.suggestAccounts no match"
        [] (Assist.suggestAccounts (T.pack "zzzznomatch"))

-- ================================================================
-- Land 1 registry: frozen pre-registry behaviour.
-- ================================================================

goldenCommit :: T.Text
goldenCommit = T.pack "2d9164642f2862725c653e496976770f6e2c7d6f"

goldenHeader :: T.Text -> T.Text
goldenHeader what =
    T.pack "# pre-land1 " <> what <> T.pack "; commit " <> goldenCommit <> T.pack "\n"

goldenShow :: Show a => a -> T.Text
goldenShow = T.pack . show

goldenEsc :: T.Text -> T.Text
goldenEsc = T.replace (T.pack "\t") (T.pack "\\t")
          . T.replace (T.pack "\n") (T.pack "\\n")

goldenDedupSort :: [T.Text] -> [T.Text]
goldenDedupSort = L.map L.head . L.group . L.sort

legacySuggestAccounts :: [Assist.AccountInfo] -> T.Text -> [Assist.AccountInfo]
legacySuggestAccounts infos query
    | L.null tokens = []
    | otherwise = L.map snd
        . L.sortOn (\(rank, info) -> (negate rank, fromEnum (Assist.aiTitle info)))
        . L.filter ((> 0) . fst)
        $ [ (matchRank info, info) | info <- infos ]
  where
    tokens = L.map T.toCaseFold (T.words query)
    matchRank info = L.length
        [ token
        | token <- tokens
        , L.any (T.isInfixOf token) (legacySearchFields info)
        ]
    legacySearchFields info = L.map T.toCaseFold $ case Registry.accountSpec (Assist.aiTitle info) of
        Just spec ->
            [ goldenShow (Assist.aiTitle info)
            , Registry.asNameEn spec
            , Registry.asNameJa spec
            , Registry.asDescription spec
            ]
        Nothing -> []

goldenInfoRow :: Assist.AccountInfo -> T.Text
-- Historical schema reconstruction. The live Land 1 AccountInfo projection is
-- pinned separately by accountMetadataLand1InfoRow.
goldenInfoRow info = case Registry.accountSpec (Assist.aiTitle info) of
    Nothing -> error "goldenInfoRow: wildcard AccountTitle"
    Just spec -> T.intercalate (T.pack "\t")
        [ goldenShow (Assist.aiTitle info)
        , goldenShow (Registry.asDivision spec)
        , goldenShow (whichSide (Not :< Assist.aiTitle info))
        , goldenEsc (Registry.asNameEn spec)
        , goldenEsc (Registry.asNameJa spec)
        , goldenEsc (Registry.asDescription spec)
        ]

goldenAliasResolution :: T.Text -> T.Text
goldenAliasResolution fixture =
    goldenHeader (T.pack "parseAccountTitle over corpus (query, show(Either ConvError AccountTitles))")
    <> T.unlines (L.map row queries)
  where
    queries = L.map (T.takeWhile (/= '\t')) (L.drop 1 (T.lines fixture))
    row query = goldenEsc query <> T.pack "\t"
             <> goldenEsc (goldenShow (EC.parseAccountTitle query))

goldenSuggestions :: T.Text
goldenSuggestions =
    goldenHeader (T.pack "suggestAccounts over corpus (query, total matches, top-10 titles)")
    <> T.unlines (L.map row corpus)
  where
    -- The post-Land2 fixture is a closed diff over the pre-vocabulary 116
    -- titles. V-Land 2 appends new titles, which are tested separately and
    -- must not retroactively change this historical fuzzy-suggestion oracle.
    infos = L.take 116 Assist.allAccountInfos
    historicalTitles = L.map Assist.aiTitle infos
    nameFields = L.concatMap legacyNameFields infos
    descTokens = L.concatMap legacyDescTokens infos
    corpus = goldenDedupSort
        (L.concatMap (\q -> [q, T.toLower q]) nameFields <> descTokens)
    row query =
        let matches = L.filter (`L.elem` historicalTitles)
                    (L.map Assist.aiTitle (legacySuggestAccounts infos query))
        in goldenEsc query <> T.pack "\t"
           <> goldenShow (L.length matches) <> T.pack "\t"
           <> T.intercalate (T.pack ",") (L.map goldenShow (L.take 10 matches))
    legacyNameFields info = case Registry.accountSpec (Assist.aiTitle info) of
        Just spec ->
            [ goldenShow (Assist.aiTitle info)
            , Registry.asNameEn spec
            , Registry.asNameJa spec
            ]
        Nothing -> []
    legacyDescTokens info = case Registry.accountSpec (Assist.aiTitle info) of
        Just spec -> T.words (Registry.asDescription spec)
        Nothing -> []

postVocabHeader :: T.Text -> T.Text
postVocabHeader what = T.pack "# post-vocab " <> what <> T.pack "; schema 1\n"

postVocabInfoGolden :: T.Text
postVocabInfoGolden =
    postVocabHeader (T.pack "AccountInfo (title, division, homeSide, nameEn, nameJa, description)")
    <> T.unlines (L.map goldenInfoRow Assist.allAccountInfos)

postVocabSuggestionsGolden :: T.Text
postVocabSuggestionsGolden =
    postVocabHeader (T.pack "suggestAccounts (query, total matches, top-10 titles)")
    <> T.unlines (L.map row corpus)
  where
    infos = Assist.allAccountInfos
    nameFields = L.concatMap legacyNameFields infos
    descTokens = L.concatMap legacyDescTokens infos
    corpus = goldenDedupSort
        (L.concatMap (\q -> [q, T.toLower q]) nameFields <> descTokens)
    row query =
        let matches = L.map Assist.aiTitle (legacySuggestAccounts infos query)
        in goldenEsc query <> T.pack "\t"
           <> goldenShow (L.length matches) <> T.pack "\t"
           <> T.intercalate (T.pack ",") (L.map goldenShow (L.take 10 matches))
    legacyNameFields info = case Registry.accountSpec (Assist.aiTitle info) of
        Just spec ->
            [ goldenShow (Assist.aiTitle info)
            , Registry.asNameEn spec
            , Registry.asNameJa spec
            ]
        Nothing -> []
    legacyDescTokens info = case Registry.accountSpec (Assist.aiTitle info) of
        Just spec -> T.words (Registry.asDescription spec)
        Nothing -> []

postVocabOrdinalsGolden :: T.Text
postVocabOrdinalsGolden =
    postVocabHeader (T.pack "Enum ordinals (constructor, fromEnum)")
    <> T.unlines
        [ goldenShow title <> T.pack "\t" <> goldenShow (fromEnum title)
        | title <- [minBound .. maxBound] :: [AccountTitles]
        ]

postVocabSemanticsGolden :: T.Text
postVocabSemanticsGolden =
    postVocabHeader (T.pack "semantics (title, whatDiv, isContra, whichSide Not, whichSide Hat, whatPIMO, fixedCurrent, finalStockProbe)")
    <> T.unlines (L.map row Registry.concreteAccountTitles)
  where
    row title =
        let nb = Not :< title :: HatBase AccountTitles
            hb = Hat :< title :: HatBase AccountTitles
        in T.intercalate (T.pack "\t")
            [ goldenShow title
            , goldenShow (whatDiv nb)
            , goldenShow (Registry.classifyAccountContra title)
            , goldenShow (whichSide nb)
            , goldenShow (whichSide hb)
            , goldenShow (whatPIMO nb)
            , goldenShow (fixedCurrent nb)
            , T.pack (finalStockProbeRule title)
            ]

testPostVocabGolden :: IO ()
testPostVocabGolden = do
    ordinals <- TIO.readFile "test/fixtures/post-vocab/ordinals.tsv"
    semantics <- TIO.readFile "test/fixtures/post-vocab/semantics.tsv"
    info <- TIO.readFile "test/fixtures/post-vocab/account-info.tsv"
    suggestions <- TIO.readFile "test/fixtures/post-vocab/suggest.tsv"
    assertEqual "post-vocab ordinal fixture" ordinals postVocabOrdinalsGolden
    assertEqual "post-vocab semantics fixture" semantics postVocabSemanticsGolden
    assertEqual "post-vocab account-info fixture" info postVocabInfoGolden
    assertEqual "post-vocab suggest fixture" suggestions postVocabSuggestionsGolden

-- ================================================================
-- 0.5.0.0 account-semantics pipeline: pre-change compatibility baseline.
-- ================================================================

accountSemanticsBaselineCommit :: T.Text
accountSemanticsBaselineCommit = T.pack "0d8e2791429145f2a48c79adbe62563328ee5c0b"

accountSemanticsHeader :: T.Text -> T.Text
accountSemanticsHeader what =
    T.pack "# pre-account-semantics-050 " <> what
    <> T.pack "; schema 1; commit " <> accountSemanticsBaselineCommit <> T.pack "\n"

accountSemanticsBinaryHex :: AccountTitles -> T.Text
accountSemanticsBinaryHex = T.pack . concatMap hexByte . BL.unpack . Binary.encode
  where
    hexByte byte = case showHex byte "" of
        [digit] -> ['0', digit]
        digits  -> digits

accountSemanticsSemanticsGolden :: T.Text
accountSemanticsSemanticsGolden =
    accountSemanticsHeader (T.pack "semantics (title, enum, binaryHex, division, closing, isContra, whichSide Not, whichSide Hat, whatPIMO, fixedCurrent, finalStockProbe)")
    <> T.unlines (L.map row Registry.concreteAccountTitles)
  where
    row title =
        let nb = Not :< title :: HatBase AccountTitles
            hb = Hat :< title :: HatBase AccountTitles
            spec = case Registry.accountSpec title of
                Just value -> value
                Nothing -> error ("missing AccountSpec for " ++ show title)
        in T.intercalate (T.pack "\t")
            [ goldenShow title
            , goldenShow (fromEnum title)
            , accountSemanticsBinaryHex title
            , goldenShow (Registry.asDivision spec)
            , goldenShow (Registry.asClosing spec)
            , goldenShow (Registry.asIsContra spec)
            , goldenShow (whichSide nb)
            , goldenShow (whichSide hb)
            , goldenShow (whatPIMO nb)
            , goldenShow (fixedCurrent nb)
            , T.pack (finalStockProbeRule title)
            ]

accountSemanticsInfoGolden :: T.Text
accountSemanticsInfoGolden =
    accountSemanticsHeader (T.pack "AccountInfo (title, division, homeSide, nameEn, nameJa, description)")
    <> T.unlines (L.map goldenInfoRow Assist.allAccountInfos)

accountSemanticsProjectionGolden :: T.Text
accountSemanticsProjectionGolden =
    accountSemanticsHeader (T.pack "projection flags for Not then Hat (currentAssets, fixedAssets, deferredAssets, currentLiability, fixedLiability, capitalStock, contraAssets, contra)")
    <> T.unlines (L.map row Registry.concreteAccountTitles)
  where
    kept :: (EA.Alg Double (HatBase AccountTitles)
          -> EA.Alg Double (HatBase AccountTitles))
         -> EA.Alg Double (HatBase AccountTitles)
         -> T.Text
    kept projection value = if norm (projection value) == (1 :: Double)
        then T.pack "1" else T.pack "0"
    row title = T.intercalate (T.pack "\t")
        (goldenShow title : L.concatMap (probe title) [Not, Hat])
    probe title hat =
        let value = 1 .@ hat :< title :: EA.Alg Double (HatBase AccountTitles)
        in [ kept EA.projCurrentAssets value
           , kept EA.projFixedAssets value
           , kept EA.projDeferredAssets value
           , kept EA.projCurrentLiability value
           , kept EA.projFixedLiability value
           , kept EA.projCapitalStock value
           , kept EA.projContraAssets value
           , kept EA.projContra value
           ]

accountSemanticsPresentationGolden :: T.Text
accountSemanticsPresentationGolden =
    accountSemanticsHeader (T.pack "legacy presentation probe (title, bsRows of 1@Not, plRows of 1@Not)")
    <> T.unlines (L.map row Registry.concreteAccountTitles)
  where
    row title =
        let value = 1 .@ Not :< title :: EA.Alg Double (HatBase AccountTitles)
        in T.intercalate (T.pack "\t")
            [ goldenShow title
            , goldenEsc (goldenShow (EW.bsRows value))
            , goldenEsc (goldenShow (EW.plRows value))
            ]

testAccountSemanticsPrechangeGolden :: IO ()
testAccountSemanticsPrechangeGolden = do
    semantics <- TIO.readFile "test/fixtures/pre-account-semantics-050/semantics.tsv"
    info <- TIO.readFile "test/fixtures/pre-account-semantics-050/account-info.tsv"
    projections <- TIO.readFile "test/fixtures/pre-account-semantics-050/projection-membership.tsv"
    presentation <- TIO.readFile "test/fixtures/pre-account-semantics-050/presentation.tsv"
    assertEqual "pre-account-semantics semantics fixture has 232 rows"
        232 (L.length (L.drop 1 (T.lines semantics)))
    assertEqual "pre-account-semantics semantics fixture"
        semantics accountSemanticsSemanticsGolden
    assertEqual "pre-account-semantics legacy AccountInfo fixture"
        info accountSemanticsInfoGolden
    assertEqual "pre-account-semantics projection fixture"
        projections accountSemanticsProjectionGolden
    assertEqual "pre-account-semantics presentation fixture"
        presentation accountSemanticsPresentationGolden

-- Land 2 (Definition 7 contra amendment) 以降: alias 解決だけが byte 一致
-- (parseAccountTitle は division 非依存)。semantics / info / suggest は
-- 意図的差分を持つため, closed-diff test (testLand2*ClosedDiff) が引き継ぐ。
testRegistryGolden :: IO ()
testRegistryGolden = do
    aliases <- TIO.readFile "test/fixtures/pre-land1/alias-resolution.tsv"
    jcci <- TIO.readFile "test/fixtures/jcci-2022/queries.tsv"
    let oldRows = L.filter (not . T.null) (L.drop 1 (T.lines aliases))
        currentRows = L.filter (not . T.null)
            (L.drop 1 (T.lines (goldenAliasResolution aliases)))
        changedQueries =
            [ T.takeWhile (/= '\t') old
            | (old, current) <- L.zip oldRows currentRows
            , old /= current
            ]
        expectedChangedQueries = L.sort (L.map T.pack
            [ "未払金", "借入金", "仮払金", "仮受金"
            , "有価証券", "投資有価証券"
            , "  未払金  ", "  借入金  ", "  仮払金  ", "  仮受金  "
            , "  有価証券  ", "  投資有価証券  "
            ])
        officialQueries =
            [ EC.normalizeTitle (fields L.!! 3)
            | line <- L.filter (not . T.null) (L.drop 1 (T.lines jcci))
            , let fields = T.splitOn (T.pack "\t") line
            , L.length fields == 7
            ]
    assertEqual "registry golden: historical row count"
        (L.length oldRows) (L.length currentRows)
    assertEqual "registry golden: every changed historical alias is JCCI-scoped"
        ([] :: [T.Text])
        [q | q <- changedQueries, EC.normalizeTitle q `L.notElem` officialQueries]
    -- The pre-land fixture freezes each old result; testJcciAccountNameCoverage
    -- freezes each new result. Freezing this exact query set closes the diff.
    assertEqual "registry golden: exact adjudicated historical alias diff"
        expectedChangedQueries (L.sort changedQueries)

-- | JCCI 2022 A欄/B欄の全 distinct query は, 一意のRightかfixtureで候補を
-- 閉じたAmbiguousのどちらかでなければならない. Unknown/first-matchは不可.
testJcciAccountNameCoverage :: IO ()
testJcciAccountNameCoverage = do
    source <- TIO.readFile "test/fixtures/jcci-2022/source.tsv"
    fixture <- TIO.readFile "test/fixtures/jcci-2022/queries.tsv"
    let sourceRows = L.filter (not . T.null) (L.drop 1 (T.lines source))
        rows = L.filter (not . T.null) (L.drop 1 (T.lines fixture))
        outcomes = [field L.!! 4 | row <- rows, let field = T.splitOn (T.pack "\t") row]
        sourceFields =
            [ fields
            | row <- sourceRows
            , let fields = T.splitOn (T.pack "\t") row
            , L.length fields == 6
            ]
        sourceEntries =
            [ (EC.normalizeTitle label, standardName)
            | fields <- sourceFields
            , let standardName = fields L.!! 2
            , label <- standardName : T.splitOn (T.pack "|") (fields L.!! 3)
            , not (T.null (T.strip label))
            ]
        sourceLabels = goldenDedupSort (L.map fst sourceEntries)
        fixtureLabels = goldenDedupSort
            [ EC.normalizeTitle (fields L.!! 3)
            | row <- rows
            , let fields = T.splitOn (T.pack "\t") row
            , L.length fields == 7
            ]
    assertEqual "JCCI source A-row count" 215 (L.length sourceRows)
    assertEqual "JCCI source rows have six columns"
        (L.length sourceRows) (L.length sourceFields)
    assertEqual "JCCI distinct normalized A/B query count" 316 (L.length rows)
    assertEqual "JCCI fixture covers exactly the source A/B labels"
        sourceLabels fixtureLabels
    assertEqual "JCCI unique resolutions" 295 (L.length (L.filter (== T.pack "right") outcomes))
    assertEqual "JCCI policy ambiguities" 21 (L.length (L.filter (== T.pack "ambiguous") outcomes))
    forM_ rows $ \row -> case T.splitOn (T.pack "\t") row of
        [_, _, _, query, _, _, standardNames] ->
            assertEqual ("JCCI source provenance: " ++ T.unpack query)
                (goldenDedupSort
                    [ standardName
                    | (label, standardName) <- sourceEntries
                    , label == EC.normalizeTitle query
                    ])
                (goldenDedupSort (T.splitOn (T.pack "|") standardNames))
        fields -> assertEqual "JCCI fixture provenance row has seven columns"
            (7 :: Int) (L.length fields)
    mapM_ check rows
  where
    check row = case T.splitOn (T.pack "\t") row of
        [_, _, _, query, outcome, candidateText, _] ->
            let names = T.splitOn (T.pack "|") candidateText
            in case traverse (`M.lookup` land2TitleMap) names of
                Nothing -> assertEqual "JCCI fixture names only real constructors"
                    (T.pack "known constructors") candidateText
                Just candidates -> case candidates of
                    [candidate] | outcome == T.pack "right" ->
                        assertEqual ("JCCI Right: " ++ T.unpack query)
                            (Right candidate) (EC.parseAccountTitle query)
                    _ : _ : _ | outcome == T.pack "ambiguous" ->
                        assertEqual ("JCCI Ambiguous: " ++ T.unpack query)
                            (Left (EC.AmbiguousAccount query candidates))
                            (EC.parseAccountTitle query)
                    _ -> assertEqual "JCCI fixture outcome/candidate arity"
                        (T.pack "right=1 or ambiguous>=2")
                        (outcome <> T.pack ":" <> candidateText)
        _ -> assertEqual "JCCI fixture row has seven columns" (7 :: Int)
            (L.length (T.splitOn (T.pack "\t") row))

testRegistryWildcards :: IO ()
testRegistryWildcards = do
    divisionResult <- try (evaluate (classifyAccountDivision AccountTitle))
        :: IO (Either SomeException AccountDivision)
    assertEqual "registry wildcard: classifyAccountDivision errors"
        True (case divisionResult of Left _ -> True; Right _ -> False)
    assertEqual "registry wildcard: fixedCurrent is Other"
        Other (fixedCurrent (Not :< AccountTitle))
    assertEqual "registry wildcard: describeAccount is Nothing"
        Nothing (Assist.describeAccount AccountTitle)

testRegistryContraLand2 :: IO ()
testRegistryContraLand2 =
    assertEqual "registry contra True set = valuation accounts plus V-Land 2 P/L contra accounts"
        allContra
        (L.filter Registry.classifyAccountContra Registry.concreteAccountTitles)

-- ================================================================
-- Land 2 (Definition 7 contra amendment): closed-diff vs pre-land1
-- golden, contract / relation properties, presentation invariance.
-- pre-land1 fixtures stay frozen as the pre-amendment reference.
-- ================================================================

land2Contra :: [AccountTitles]
land2Contra = [AllowanceForDoubtfulAccounts, AccumulatedDepreciation]

allContra :: [AccountTitles]
allContra = land2Contra <> [SalesRebates, RefundOfIncomeTaxes, PurchaseRebates]

land2TitleMap :: M.Map T.Text AccountTitles
land2TitleMap = M.fromList
    [ (T.pack (show t), t) | t <- Registry.concreteAccountTitles ]

-- registry から生成しない literal 期待値 (循環 oracle 回避)
land2ExpectedDesc :: AccountTitles -> T.Text
land2ExpectedDesc AllowanceForDoubtfulAccounts = T.pack
    "Asset (contra): Allowance for doubtful accounts (貸倒引当金), a credit-balance valuation account (評価勘定) deducted from receivables. Home side is Credit because it is a contra asset (isContra); values stay non-negative and the Hat\\/Not structure is intact. B\\/S deduction (net) presentation is the Write side's job."
land2ExpectedDesc AccumulatedDepreciation = T.pack
    "Asset (contra): Accumulated depreciation (減価償却累計額), a credit-balance valuation account (評価勘定) under the indirect method (間接法), deducted from the related depreciable assets. Home side is Credit because it is a contra asset (isContra). This is the canonical bookkeeping account for accumulated depreciation; the existing 'ReserveForDepreciation' is retained as the legacy SNA\\/macro-accounting name."
land2ExpectedDesc t = T.pack ("land2ExpectedDesc: not a contra account: " ++ show t)

-- T1: 全域機械比較 — whichSide/whatPIMO/fixedCurrent は全一致,
-- whatDiv は当該 2 件 (Liability→Assets) ちょうど。
testLand2SemanticsClosedDiff :: IO ()
testLand2SemanticsClosedDiff = do
    fixture <- TIO.readFile "test/fixtures/pre-land1/account-semantics.tsv"
    let rows = L.filter (not . T.null) (L.drop 1 (T.lines fixture))
    assertEqual "land2 semantics: fixture row count" 116 (L.length rows)
    mapM_ checkRow rows
  where
    checkRow line = case T.splitOn (T.pack "\t") line of
        [name, oldDiv, oldPimo, oldSideN, oldSideH, oldFc] ->
            case M.lookup name land2TitleMap of
                Nothing -> assertEqual "land2 semantics: unknown fixture title" (T.pack "") name
                Just t -> do
                    let nb = Not :< t :: HatBase AccountTitles
                        hb = Hat :< t :: HatBase AccountTitles
                    assertEqual ("land2 whatPIMO invariant: " ++ show t)
                        oldPimo (goldenShow (whatPIMO nb))
                    assertEqual ("land2 whichSide Not invariant: " ++ show t)
                        oldSideN (goldenShow (whichSide nb))
                    assertEqual ("land2 whichSide Hat invariant: " ++ show t)
                        oldSideH (goldenShow (whichSide hb))
                    assertEqual ("land2 fixedCurrent invariant: " ++ show t)
                        oldFc (goldenShow (fixedCurrent nb))
                    if t `L.elem` land2Contra
                        then do
                            assertEqual ("land2 whatDiv old was Liability: " ++ show t)
                                (T.pack "Liability") oldDiv
                            assertEqual ("land2 whatDiv new is Assets: " ++ show t)
                                Assets (whatDiv nb)
                        else assertEqual ("land2 whatDiv invariant: " ++ show t)
                                oldDiv (goldenShow (whatDiv nb))
        _ -> assertEqual "land2 semantics: malformed fixture row" (T.pack "") line

-- T8 込み: allAccountInfos の閉じた差分 — 当該 2 行だけ aiDivision と
-- aiDesc が変わり (desc は "Asset (contra):" で始まる), 他は byte 一致。
testLand2InfoClosedDiff :: IO ()
testLand2InfoClosedDiff = do
    fixture <- TIO.readFile "test/fixtures/pre-land1/account-info.tsv"
    let oldRows = L.filter (not . T.null) (L.drop 1 (T.lines fixture))
        -- V-Land 2 appends new constructors after every pre-vland2 concrete
        -- title. This closed-diff compares only the pinned historical prefix.
        newRows = L.take (L.length oldRows)
            [ (Assist.aiTitle i, goldenInfoRow i) | i <- Assist.allAccountInfos ]
    assertEqual "land2 info: row count" (L.length oldRows) (L.length newRows)
    mapM_ check (L.zip oldRows newRows)
  where
    check (oldLine, (t, newLine))
        | t `L.elem` land2Contra = do
            let oldF = T.splitOn (T.pack "\t") oldLine
                newF = T.splitOn (T.pack "\t") newLine
            assertEqual ("land2 info title invariant: " ++ show t)
                (oldF L.!! 0) (newF L.!! 0)
            assertEqual ("land2 info old division was Liability: " ++ show t)
                (T.pack "Liability") (oldF L.!! 1)
            assertEqual ("land2 info new division is Assets: " ++ show t)
                (T.pack "Assets") (newF L.!! 1)
            assertEqual ("land2 info home side invariant: " ++ show t)
                (oldF L.!! 2) (newF L.!! 2)
            assertEqual ("land2 info nameEn invariant: " ++ show t)
                (oldF L.!! 3) (newF L.!! 3)
            assertEqual ("land2 info nameJa invariant: " ++ show t)
                (oldF L.!! 4) (newF L.!! 4)
            assertEqual ("land2 info desc updated to contra wording: " ++ show t)
                True (oldF L.!! 5 /= newF L.!! 5)
            assertEqual ("land2 info new desc literal: " ++ show t)
                (land2ExpectedDesc t) (newF L.!! 5)
        | otherwise =
            assertEqual ("land2 info invariant: " ++ show t) oldLine newLine

-- suggest の閉じた差分: 変化した (追加/削除/変更) query は全て, 当該 2 科目の
-- 旧/新 desc に対する token match rank の変化で説明できる。
testLand2SuggestClosedDiff :: IO ()
testLand2SuggestClosedDiff = do
    fixture <- TIO.readFile "test/fixtures/pre-land1/suggest.tsv"
    infoFixture <- TIO.readFile "test/fixtures/pre-land1/account-info.tsv"
    let toMap txt = M.fromList
            [ (T.takeWhile (/= '\t') line, line)
            | line <- L.filter (not . T.null) (L.drop 1 (T.lines txt)) ]
        oldMap = toMap fixture
        newMap = toMap goldenSuggestions
        oldFieldsOf t = L.concat
            [ [ fs L.!! 0, fs L.!! 3, fs L.!! 4, fs L.!! 5 ]
            | line <- L.filter (not . T.null) (L.drop 1 (T.lines infoFixture))
            , let fs = T.splitOn (T.pack "\t") line
            , fs L.!! 0 == T.pack (show t)
            ]
        newFieldsOf t = case Assist.describeAccount t of
            Just i  -> [ T.pack (show t), Assist.aiNameEn i, Assist.aiNameJa i, Assist.aiDesc i ]
            Nothing -> []
        rank fields q = L.length
            [ tok
            | tok <- L.map T.toCaseFold (T.words q)
            , L.any (T.isInfixOf tok) (L.map T.toCaseFold fields) ]
        -- corpus 帰属の変化 (query が旧/新 desc の token 集合の片方にだけある)
        -- も desc 変更の帰結として許容する (行の追加/削除がこれで起きる)。
        descTokensOf fields = case fields of
            [_, _, _, desc] -> T.words desc
            _               -> []
        tokenMembershipChange q = L.or
            [ (q `L.elem` descTokensOf (oldFieldsOf t))
              /= (q `L.elem` descTokensOf (newFieldsOf t))
            | t <- land2Contra ]
        affected q = tokenMembershipChange q || L.or
            [ rank (oldFieldsOf t) q /= rank (newFieldsOf t) q | t <- land2Contra ]
        diffQueries = L.nub
            (  [ q | (q, old) <- M.toList oldMap, maybe True (/= old) (M.lookup q newMap) ]
            ++ [ q | q <- M.keys newMap, not (M.member q oldMap) ] )
    postFixture <- TIO.readFile "test/fixtures/post-land2/suggest.tsv"
    assertEqual "land2 suggest: post fixture byte-identical (expected output itself)"
        postFixture goldenSuggestions
    assertEqual "land2 suggest: some diff exists (descs changed)"
        True (not (L.null diffQueries))
    mapM_ (\q -> assertEqual
              ("land2 suggest diff explained by contra desc change: " ++ T.unpack q)
              True (affected q))
          diffQueries

-- T2: 契約 isContra(b) ⇔ homeSide(b) ≠ defaultSide(whatDiv b)
testLand2Contract :: IO ()
testLand2Contract = mapM_ check Registry.concreteAccountTitles
  where
    check t =
        let nb = Not :< t :: HatBase AccountTitles
        in assertEqual ("land2 contract (isContra = reversed home side): " ++ show t)
            (isContra nb) (whichSide nb /= defaultSide (whatDiv nb))

-- T3: pimoFlip は自己逆で, 原典の交換関係を保つ
testLand2PimoFlip :: IO ()
testLand2PimoFlip = do
    let pimoAll = [PS, IN, MS, OUT]
    mapM_ (\x -> assertEqual ("land2 pimoFlip involution: " ++ show x)
              x (pimoFlip (pimoFlip x))) pimoAll
    mapM_ (\(x, y) -> assertEqual ("land2 pimoFlip preserves (<=>): " ++ show (x, y))
              (x <=> y) (pimoFlip x <=> pimoFlip y))
          [ (x, y) | x <- pimoAll, y <- pimoAll ]

-- T4: (<=>) — PIMO instance = 原典 Prop 5.3.8, Division instance =
-- pimoFromDivision 経由の外延, 旧 instance との差分 = ordered 4 case ちょうど。
testLand2ExchangeRelation :: IO ()
testLand2ExchangeRelation = do
    let pimoAll = [PS, IN, MS, OUT]
        divAll  = [Assets, Equity, Liability, Cost, Revenue]
    assertEqual "land2 (<=>) PIMO instance = Prop 5.3.8 pairs"
        [ (PS,IN), (PS,MS), (IN,PS), (IN,OUT), (MS,PS), (MS,OUT), (OUT,IN), (OUT,MS) ]
        [ (x, y) | x <- pimoAll, y <- pimoAll, x <=> y ]
    mapM_ (\(a, b) -> assertEqual ("land2 (<=>) division = via pimoFromDivision: " ++ show (a, b))
              (pimoFromDivision a <=> pimoFromDivision b) (a <=> b))
          [ (a, b) | a <- divAll, b <- divAll ]
    let oldRel (Assets, Liability) = True
        oldRel (Liability, Assets) = True
        oldRel (Assets, Equity)    = True
        oldRel (Equity, Assets)    = True
        oldRel (Cost, Liability)   = True
        oldRel (Liability, Cost)   = True
        oldRel (Cost, Equity)      = True
        oldRel (Equity, Cost)      = True
        oldRel _                   = False
    assertEqual "land2 (<=>) division migration = exactly 4 ordered cases"
        [ (Assets, Revenue), (Cost, Revenue), (Revenue, Assets), (Revenue, Cost) ]
        [ (a, b) | a <- divAll, b <- divAll, oldRel (a, b) /= (a <=> b) ]

-- T9: 8 組込み instance + custom instance (SimHatBase2) の全科目 sweepで
-- isContra のTrue集合が既存評価勘定2件 + V-Land 2 P/L控除3件になること。
testLand2IsContraInstances :: IO ()
testLand2IsContraInstances = do
    let day0 = fromGregorian 2026 1 1
        tod0 = TimeOfDay 0 0 0
        nm   = T.pack "spec"
        sweep :: ExBaseClass b => String -> (AccountTitles -> b) -> IO ()
        sweep label mk = assertEqual ("land2 isContra sweep: " ++ label)
            allContra
            [ t | t <- Registry.concreteAccountTitles, isContra (mk t) ]
    sweep "HatBase AccountTitles" (\t -> Not :< t :: HatBase AccountTitles)
    sweep "HatBase (AccountTitles, Day)" (\t -> Not :< (t, day0))
    sweep "HatBase (AccountTitles, Name)" (\t -> Not :< (t, nm))
    sweep "HatBase (CountUnit, AccountTitles)" (\t -> Not :< (Yen, t))
    sweep "HatBase (AccountTitles, Name, CountUnit)" (\t -> Not :< (t, nm, Yen))
    sweep "HatBase (AccountTitles, Name, CountUnit, Subject)"
          (\t -> Not :< (t, nm, Yen, nm))
    sweep "HatBase (AccountTitles, Name, CountUnit, Subject, Day)"
          (\t -> Not :< (t, nm, Yen, nm, day0))
    sweep "HatBase (AccountTitles, Name, CountUnit, Subject, Day, TimeOfDay)"
          (\t -> Not :< (t, nm, Yen, nm, day0, tod0))
    sweep "SimHatBase2 (custom instance)" (\t -> Not :< (t, 1, 2, Yen) :: SimHatBase2)

-- T8: LLM-facing メタデータの literal 期待値 (registry から生成しない)
testLand2AiDivision :: IO ()
testLand2AiDivision = do
    assertEqual "land2 statement metadata literal: AllowanceForDoubtfulAccounts"
        (Just (StatementDivision Assets, FixedHomeSide Credit))
        (fmap (\i -> (Assist.aiDivisionSemantics i, Assist.aiHomeSideSemantics i))
              (Assist.describeAccount AllowanceForDoubtfulAccounts))
    assertEqual "land2 statement metadata literal: AccumulatedDepreciation"
        (Just (StatementDivision Assets, FixedHomeSide Credit))
        (fmap (\i -> (Assist.aiDivisionSemantics i, Assist.aiHomeSideSemantics i))
              (Assist.describeAccount AccumulatedDepreciation))

-- T5/T6: presentation battery。bsRows/plRows の literal は, 以下で明記する
-- vocabulary closing 差分を除き, Land 1 出力 (pre-land2 golden, commit
-- 1c1f3f2) と byte 一致 = 表示互換 shim の証明。
-- division projection は contra を含まず, contra は projContraAssets のみが選ぶ
-- (意図的差分: projCurrentLiability/projFixedLiability から当該 2 件が消えた)。
land2B1, land2B2, land2B3, land2B4, land2B5 :: BAlg
land2B1 = 100 .@ Not:<Cash .+ 60 .@ Not:<LoansPayable .+ 40 .@ Not:<CapitalStock
land2B2 = 500 .@ Not:<Sales .+ 300 .@ Not:<SalesCost
land2B3 = 1000 .@ Not:<AccountsReceivable .+ 900 .@ Not:<Cash .+ 800 .@ Not:<Building
  .+ 100 .@ Not:<AllowanceForDoubtfulAccounts .+ 200 .@ Not:<AccumulatedDepreciation
  .+ 2000 .@ Not:<CapitalStock .+ 400 .@ Not:<LoansPayable
land2B4 = 30 .@ Not:<Cash .+ 80 .@ Hat:<Cash
  .+ 100 .@ Not:<AllowanceForDoubtfulAccounts .+ 120 .@ Hat:<AllowanceForDoubtfulAccounts
  .+ 500 .@ Not:<Building .+ 200 .@ Not:<LoansPayable .+ 300 .@ Hat:<LoansPayable
  .+ 250 .@ Not:<AccumulatedDepreciation .+ 50 .@ Hat:<AccumulatedDepreciation
land2B5 = land2B3 .+ 300 .@ Not:<SalesCost .+ 500 .@ Not:<Sales .+ 200 .@ Not:<Cash

testLand2Presentation :: IO ()
testLand2Presentation = do
    let rows f b = L.map (L.map T.unpack) (f b)
    -- bsRows: Land 1 と byte 一致 (b4 は abnormal balance の既存の癖ごと保存)
    assertEqual "land2 bsRows b1 (= Land 1)"
        [ ["Asset","","Liability",""]
        , ["Cash","100.0","LoansPayable","60.0"]
        , ["Total","100.0","Equity",""]
        , ["","","CapitalStock","40.0"]
        , ["","","Total","100.0"] ]
        (rows bsRows land2B1)
    assertEqual "land2 bsRows b3 contra placement (= Land 1)"
        [ ["Asset","","Liability",""]
        , ["AccountsReceivable","1000.0","LoansPayable","400.0"]
        , ["Building","800.0","AllowanceForDoubtfulAccounts","100.0"]
        , ["Cash","900.0","AccumulatedDepreciation","200.0"]
        , ["Total","2700.0","Equity",""]
        , ["","","CapitalStock","2000.0"]
        , ["","","Total","2700.0"] ]
        (rows bsRows land2B3)
    assertEqual "land2 bsRows b4 abnormal (= Land 1, incl. known row/total quirk)"
        [ ["Asset","","Liability",""]
        , ["LoansPayable","100.0","AccumulatedDepreciation","200.0"]
        , ["AllowanceForDoubtfulAccounts","20.0","Equity",""]
        , ["Building","500.0","Total","250.0"]
        , ["Total","620.0","",""] ]
        (rows bsRows land2B4)
    -- Pre-vocab difference: SalesCost is now closed by the registry-derived
    -- Cost rule, so Sales 500 - SalesCost 300 becomes RetainedEarnings 200.
    assertEqual "land2 bsRows b5 closing (SalesCost now closes)"
        [ ["Asset","","Liability",""]
        , ["AccountsReceivable","1000.0","LoansPayable","400.0"]
        , ["Building","800.0","AllowanceForDoubtfulAccounts","100.0"]
        , ["Cash","1100.0","AccumulatedDepreciation","200.0"]
        , ["Total","2900.0","Equity",""]
        , ["","","CapitalStock","2000.0"]
        , ["","","RetainedEarnings","200.0"]
        , ["","","Total","2900.0"] ]
        (rows bsRows land2B5)
    assertEqual "land2 plRows b2 (= Land 1)"
        [ ["Cost","","Revenue",""]
        , ["SalesCost","300.0","Sales","500.0"]
        , ["Total","500.0","Total","300.0"] ]
        (rows plRows land2B2)
    -- projections: 資産系は Land 1 と一致, liability 系は contra が消える (意図的差分),
    -- contra は projContraAssets のみが Hat/Not 双方を保持して選ぶ。
    assertEqual "land2 projCurrentAssets b3 (= Land 1)"
        "900.00:@Not:<Cash .+ 1000.00:@Not:<AccountsReceivable"
        (show (EA.projCurrentAssets land2B3))
    assertEqual "land2 projCurrentLiability b3 (intentional: contra dropped)"
        "400.00:@Not:<LoansPayable"
        (show (EA.projCurrentLiability land2B3))
    assertEqual "land2 projFixedLiability b3 (intentional: contra dropped)"
        "0"
        (show (EA.projFixedLiability land2B3))
    assertEqual "land2 projContraAssets b3"
        "100.00:@Not:<AllowanceForDoubtfulAccounts .+ 200.00:@Not:<AccumulatedDepreciation"
        (show (EA.projContraAssets land2B3))
    assertEqual "land2 projCurrentAssets b4 (= Land 1; no contra Hat leakage)"
        "30.00:@Not:<Cash"
        (show (EA.projCurrentAssets land2B4))
    assertEqual "land2 projFixedAssets b4 (= Land 1; no contra Hat leakage)"
        "500.00:@Not:<Building"
        (show (EA.projFixedAssets land2B4))
    assertEqual "land2 projCurrentLiability b4 (intentional: contra dropped)"
        "200.00:@Not:<LoansPayable"
        (show (EA.projCurrentLiability land2B4))
    assertEqual "land2 projContraAssets b4 keeps Hat and Not, excludes Cash"
        "120.00:@Hat:<AllowanceForDoubtfulAccounts .+ 100.00:@Not:<AllowanceForDoubtfulAccounts .+ 50.00:@Hat:<AccumulatedDepreciation .+ 250.00:@Not:<AccumulatedDepreciation"
        (show (EA.projContraAssets land2B4))
    assertEqual "land2 projContraAssets b5"
        "100.00:@Not:<AllowanceForDoubtfulAccounts .+ 200.00:@Not:<AccumulatedDepreciation"
        (show (EA.projContraAssets land2B5))

-- pre-land2 presentation fixture (Land 1 時点の全 battery dump) との
-- 行単位 closed diff: contra division projection の既存 6 行に,
-- SalesCost の閉鎖漏れ修正から生じる b2/b5 の B/S 8 行を加えた 14 行。
land2PresentationLines :: [T.Text]
land2PresentationLines = L.concatMap sect
    [ ("b1-basic", land2B1), ("b2-pl", land2B2), ("b3-contra", land2B3)
    , ("b4-abnormal", land2B4), ("b5-closing", land2B5) ]
  where
    sect (n, a) =
        (T.pack ("## " ++ n))
      : T.pack "-- bsRows"
      : L.map (T.pack . show . L.map T.unpack) (bsRows a)
     ++ T.pack "-- plRows"
      : L.map (T.pack . show . L.map T.unpack) (plRows a)
     ++ L.concat [ [T.pack ("-- " ++ pn), T.pack (show (pf a))] | (pn, pf) <- projList ]
    projList =
        [ ("projCurrentAssets",    EA.projCurrentAssets)
        , ("projFixedAssets",      EA.projFixedAssets)
        , ("projDeferredAssets",   EA.projDeferredAssets)
        , ("projCurrentLiability", EA.projCurrentLiability)
        , ("projFixedLiability",   EA.projFixedLiability)
        , ("projCapitalStock",     EA.projCapitalStock)
        ]

testLand2PresentationClosedDiff :: IO ()
testLand2PresentationClosedDiff = do
    fixture <- TIO.readFile "test/fixtures/pre-land2/presentation.txt"
    let oldLines = L.filter (not . T.null) (L.drop 1 (T.lines fixture))
        newLines = land2PresentationLines
    assertEqual "land2 presentation: line count" (L.length oldLines) (L.length newLines)
    assertEqual "land2 presentation closed diff = contra projections plus SalesCost closing"
        [ (T.pack "[\"SalesCost\",\"300.0\",\"Equity\",\"\"]", T.pack "[\"Total\",\"0.0\",\"Equity\",\"\"]")
        , (T.pack "[\"Total\",\"300.0\",\"RetainedEarnings\",\"500.0\"]", T.pack "[\"\",\"\",\"RetainedEarnings\",\"200.0\"]")
        , (T.pack "[\"\",\"\",\"Total\",\"500.0\"]", T.pack "[\"\",\"\",\"Total\",\"200.0\"]")
        , (T.pack "400.00:@Not:<LoansPayable .+ 100.00:@Not:<AllowanceForDoubtfulAccounts", T.pack "400.00:@Not:<LoansPayable")
        , (T.pack "200.00:@Not:<AccumulatedDepreciation", T.pack "0")
        , (T.pack "200.00:@Not:<LoansPayable .+ 100.00:@Not:<AllowanceForDoubtfulAccounts", T.pack "200.00:@Not:<LoansPayable")
        , (T.pack "250.00:@Not:<AccumulatedDepreciation", T.pack "0")
        , (T.pack "[\"SalesCost\",\"300.0\",\"AccumulatedDepreciation\",\"200.0\"]", T.pack "[\"Cash\",\"1100.0\",\"AccumulatedDepreciation\",\"200.0\"]")
        , (T.pack "[\"Cash\",\"1100.0\",\"Equity\",\"\"]", T.pack "[\"Total\",\"2900.0\",\"Equity\",\"\"]")
        , (T.pack "[\"Total\",\"3200.0\",\"CapitalStock\",\"2000.0\"]", T.pack "[\"\",\"\",\"CapitalStock\",\"2000.0\"]")
        , (T.pack "[\"\",\"\",\"RetainedEarnings\",\"500.0\"]", T.pack "[\"\",\"\",\"RetainedEarnings\",\"200.0\"]")
        , (T.pack "[\"\",\"\",\"Total\",\"3200.0\"]", T.pack "[\"\",\"\",\"Total\",\"2900.0\"]")
        , (T.pack "400.00:@Not:<LoansPayable .+ 100.00:@Not:<AllowanceForDoubtfulAccounts", T.pack "400.00:@Not:<LoansPayable")
        , (T.pack "200.00:@Not:<AccumulatedDepreciation", T.pack "0")
        ]
        [ (o, n) | (o, n) <- L.zip oldLines newLines, o /= n ]

-- HatNot は whichSide で明示 error (規約の regression 固定)
testLand2HatNotPolicy :: IO ()
testLand2HatNotPolicy = do
    r <- try (evaluate (whichSide (HatNot :< Cash :: HatBase AccountTitles)))
        :: IO (Either SomeException Side)
    assertEqual "land2 whichSide HatNot policy: explicit error"
        True (case r of Left _ -> True; Right _ -> False)

-- ================================================================
-- ExchangeAlgebra.Convert.Checked: checked construction for generated entries.
-- ================================================================

type CheckedAlgM = EA.Alg MoneyDecimal (HatBase AccountTitles)
type CheckedJournalM = EJ.Journal Int MoneyDecimal (HatBase AccountTitles)

checkedEntryM :: [(Side, AccountTitles, MoneyDecimal)]
              -> Either (NE.NonEmpty (ECC.EntryError MoneyDecimal)) CheckedAlgM
checkedEntryM = ECC.checkedEntry

checkedJournalM :: [(Int, [(Side, AccountTitles, MoneyDecimal)])]
                -> Either (NE.NonEmpty (ECC.JournalError Int MoneyDecimal)) CheckedJournalM
checkedJournalM = ECC.checkedJournal

genPositiveAmountMD :: Gen MoneyDecimal
genPositiveAmountMD = fromInteger <$> choose (1, 9999)

genCheckedAmountMD :: Gen MoneyDecimal
genCheckedAmountMD = fromInteger <$> choose (-5, 20)

genCheckedSide :: Gen Side
genCheckedSide = frequency
    [ (8, elements [Debit, Credit])
    , (1, pure Side)
    ]

genCheckedAccountTitle :: Gen AccountTitles
genCheckedAccountTitle = frequency
    [ (12, genAccountTitle)
    , (1, pure AccountTitle)
    ]

genCheckedPosting :: Gen (Side, AccountTitles, MoneyDecimal)
genCheckedPosting =
    (,,) <$> genCheckedSide <*> genCheckedAccountTitle <*> genCheckedAmountMD

genAcceptedEntryRows :: Gen [(Side, AccountTitles, MoneyDecimal)]
genAcceptedEntryRows = do
    amount <- genPositiveAmountMD
    debitAccount <- genOrdinaryPostingTitle
    creditAccount <- genOrdinaryPostingTitle
    pure [ (Debit, debitAccount, amount)
         , (Credit, creditAccount, amount)
         ]

genOrdinaryPostingTitle :: Gen AccountTitles
genOrdinaryPostingTitle = elements
    [ title
    | title <- Registry.concreteAccountTitles
    , Just semantics <- [Registry.accountSemantics title]
    , Registry.asemPostingCapability semantics == OrdinaryPosting
    ]

genCheckedEntryRows :: Gen [(Side, AccountTitles, MoneyDecimal)]
genCheckedEntryRows = frequency
    [ (5, resize 8 (listOf genCheckedPosting))
    , (3, genAcceptedEntryRows)
    , (1, pure [])
    ]

genKnownCertPosting :: Gen (Side, AccountTitles, MoneyDecimal)
genKnownCertPosting =
    (,,) <$> elements [Debit, Credit] <*> genAccountTitle <*> genCheckedAmountMD

genKnownCertJournal :: Gen [(Int, [(Side, AccountTitles, MoneyDecimal)])]
genKnownCertJournal = do
    rows <- resize 6 (listOf (resize 6 (listOf genKnownCertPosting)))
    pure (zip [1..] rows)

sideTextForCert :: Side -> T.Text
sideTextForCert Debit  = T.pack "debit"
sideTextForCert Credit = T.pack "credit"
sideTextForCert Side   = T.pack "Side"

textJournalForCert
    :: [(Int, [(Side, AccountTitles, MoneyDecimal)])]
    -> [(Int, [(T.Text, T.Text, MoneyDecimal)])]
textJournalForCert = L.map renderEntry
  where
    renderEntry (txid, rows) = (txid, L.map renderPosting rows)
    renderPosting (side, account, amount) =
        (sideTextForCert side, T.pack (show account), amount)

checkedJournalTextReference
    :: [(Int, [(T.Text, T.Text, MoneyDecimal)])]
    -> Either (NE.NonEmpty (ECC.JournalError Int MoneyDecimal)) CheckedJournalM
checkedJournalTextReference entries =
    case errors of
        []     -> Right (L.foldl' (.+) mempty journals)
        e : es -> Left (e NE.:| es)
  where
    checked =
        [ (txid, ECC.checkedEntryText rows)
        | (txid, rows) <- entries
        ]
    errors =
        [ ECC.EntryErrors txid errs
        | (txid, Left errs) <- checked
        ]
    journals =
        [ alg .| txid
        | (txid, Right alg) <- checked
        ]

prop_certifyKnownAccountsMatchesCheckedJournal :: Property
prop_certifyKnownAccountsMatchesCheckedJournal =
    forAll genKnownCertJournal $ \entries ->
        let textEntries = textJournalForCert entries
        in case ( ECC.certifyJournalText textEntries
             , checkedJournalTextReference textEntries
             , checkedJournalM entries
             ) of
            (ECC.FullyResolved actual, Right textExpected, Right expected) ->
                EJ.toMap actual == EJ.toMap textExpected
                && EJ.toMap actual == EJ.toMap expected
            (ECC.Rejected _, Left _, Left _) -> True
            _                                  -> False

prop_certifyUnknownAccountPreservesBalance :: Property
prop_certifyUnknownAccountPreservesBalance =
    forAll genAcceptedEntryRows $ \rows ->
        let replaceAccount (side, _, amount) =
                (sideTextForCert side, T.pack "NoSuchAccount_XYZ", amount)
        in case ECC.certifyJournalText [(1 :: Int, L.map replaceAccount rows)] of
            ECC.BalancedUnresolved {} -> True
            _                         -> False

prop_certifyImbalancePrecedesUnknownAccount :: Property
prop_certifyImbalancePrecedesUnknownAccount =
    forAll genPositiveAmountMD $ \amount ->
        let input =
                [ (1 :: Int,
                    [ (T.pack "debit", T.pack "NoSuchAccount_XYZ", amount)
                    , (T.pack "credit", T.pack "Cash", amount + 1)
                    ])
                ]
        in case ECC.certifyJournalText input of
            ECC.Rejected errs -> any journalHasImbalance (NE.toList errs)
            _                 -> False
  where
    journalHasImbalance (ECC.EntryErrors _ errs) =
        any isImbalanced (NE.toList errs)
    journalHasImbalance _ = False

    isImbalanced ECC.Imbalanced {} = True
    isImbalanced _                 = False

prop_certifyDuplicateTxIdAlwaysRejected :: Property
prop_certifyDuplicateTxIdAlwaysRejected =
    forAll genAcceptedEntryRows $ \rows ->
        let input = textJournalForCert [(1, rows), (1, rows)]
        in case ECC.certifyJournalText input of
            ECC.Rejected errs -> ECC.DuplicateTxId 1 `elem` NE.toList errs
            _                 -> False

prop_certifyBalancedUnresolvedTotals :: Property
prop_certifyBalancedUnresolvedTotals =
    forAll genPositiveAmountMD $ \amount ->
        let input =
                [ (1 :: Int,
                    [ (T.pack "debit", T.pack "NoSuchAccount_XYZ", amount)
                    , (T.pack "credit", T.pack "Sales", amount)
                    ])
                ]
        in case ECC.certifyJournalText input of
            ECC.BalancedUnresolved
                { ECC._certDebitTotal = debitTotal
                , ECC._certCreditTotal = creditTotal
                } -> debitTotal == creditTotal
                     && debitTotal == amount
                     && creditTotal == amount
            _ -> False

checkedEntryAcceptsSpec :: [(Side, AccountTitles, MoneyDecimal)] -> Bool
checkedEntryAcceptsSpec rows =
    not (null rows)
    && all validPosting rows
    && ECC.exactBalanced (EC.journalFromSides rows :: CheckedAlgM)
  where
    validPosting (side, account, amount) =
        side /= Side
        && account /= AccountTitle
        && maybe False
            (ECC.postingAllowedIn ECC.OrdinaryJournal
                . Registry.asemPostingCapability)
            (Registry.accountSemantics account)
        && amount > 0
        && not (EA.isErrorValue amount)

data ConsolidationFixtureRow
  = FixturePosting String String [String]
        AccountTitles AccountTitles MoneyDecimal
  | FixtureLink String String MoneyDecimal
  deriving (Show, Eq)

parseConsolidationFixtureRow :: T.Text -> Either String ConsolidationFixtureRow
parseConsolidationFixtureRow line = case T.splitOn (T.pack "\t") line of
    [kind, rowId, sourceIdsText, debitText, creditText, amountText]
        | kind == T.pack "source" || kind == T.pack "adjustment" -> do
            debitAccount <- firstShow (EC.parseAccountTitle debitText)
            creditAccount <- firstShow (EC.parseAccountTitle creditText)
            amount <- parseAmount amountText
            let sourceIds
                    | sourceIdsText == T.pack "-" = []
                    | otherwise = L.map T.unpack
                        (T.splitOn (T.pack "|") sourceIdsText)
            Right (FixturePosting (T.unpack kind) (T.unpack rowId) sourceIds
                debitAccount creditAccount amount)
        | kind == T.pack "link" -> do
            amount <- parseAmount amountText
            Right (FixtureLink (T.unpack rowId) (T.unpack sourceIdsText) amount)
    fields -> Left ("invalid consolidation fixture row: " ++ show fields)
  where
    firstShow (Left err) = Left (show err)
    firstShow (Right value) = Right value
    parseAmount amountText = case reads (T.unpack amountText) of
        [(amount, "")] -> Right (fromInteger amount)
        _ -> Left ("invalid fixture amount: " ++ T.unpack amountText)

loadMinimalConsolidationFixture
    :: IO (CW.WorksheetInput String String MoneyDecimal)
loadMinimalConsolidationFixture = do
    contents <- TIO.readFile
        "test/fixtures/consolidation-worksheet-050/minimal.tsv"
    rows <- case traverse parseConsolidationFixtureRow
            (filter (not . T.null) (drop 1 (T.lines contents))) of
        Left err -> fail err
        Right parsed -> pure parsed
    let postings =
            [ (kind, rowId, sourceIds, debitAccount, creditAccount, amount)
            | FixturePosting kind rowId sourceIds debitAccount creditAccount amount
                <- rows
            ]
        sources =
            [ CW.TrialBalanceSource rowId
                (EC.journalFromSides
                    [ (Debit, debitAccount, amount)
                    , (Credit, creditAccount, amount)
                    ] :: CheckedAlgM)
            | (kind, rowId, _, debitAccount, creditAccount, amount) <- postings
            , kind == "source"
            ]
        adjustments =
            [ CW.WorksheetAdjustment rowId (sourceId NE.:| sourceIdsTail)
                (EC.journalFromSides
                    [ (Debit, debitAccount, amount)
                    , (Credit, creditAccount, amount)
                    ] :: CheckedAlgM)
            | (kind, rowId, sourceId : sourceIdsTail,
                    debitAccount, creditAccount, amount) <- postings
            , kind == "adjustment"
            ]
        links = M.fromList
            [ (rowId, (direction, amount))
            | FixtureLink rowId direction amount <- rows
            ]
    sourceList <- case NE.nonEmpty sources of
        Nothing -> fail "minimal consolidation fixture has no sources"
        Just nonEmptySources -> pure nonEmptySources
    plResult <- fixtureResult links "pl-net-income"
    ownersPlResult <- fixtureResult links "pl-parent-net-income"
    ownersSsResult <- fixtureResult links "ss-parent-net-income"
    nciResult <- fixtureResult links "nci-period-share"
    openingRetained <- fixtureBalance links "opening-retained-earnings"
    retainedDividends <- fixtureAmount links "retained-earnings-dividends"
    ssClosingRetained <- fixtureBalance links "ss-closing-retained-earnings"
    bsRetained <- fixtureBalance links "bs-retained-earnings"
    openingNci <- fixtureBalance links "opening-nci"
    nciDividends <- fixtureAmount links "nci-dividends"
    closingNci <- fixtureBalance links "closing-nci"
    bsNci <- fixtureBalance links "bs-nci"
    pure (CW.WorksheetInput sourceList adjustments
        (CW.WorksheetLinkage
            { CW._profitOrLossNetIncome = plResult
            , CW._profitOrLossNetIncomeAttributableToOwners = ownersPlResult
            , CW._statementOfChangesNetIncomeAttributableToOwners =
                ownersSsResult
            , CW._openingRetainedEarnings = openingRetained
            , CW._retainedEarningsDividends = retainedDividends
            , CW._statementOfChangesClosingRetainedEarnings = ssClosingRetained
            , CW._balanceSheetRetainedEarnings = bsRetained
            , CW._openingNonControllingInterests = openingNci
            , CW._nonControllingInterestsPeriodShare = nciResult
            , CW._nonControllingInterestsDividends = nciDividends
            , CW._statementOfChangesClosingNonControllingInterests = closingNci
            , CW._balanceSheetNonControllingInterests = bsNci
            }))
  where
    fixtureAmount links key = case M.lookup key links of
        Just ("amount", amount) -> pure amount
        Just (direction, _) -> fail
            ("expected amount direction for " ++ key ++ ", got " ++ direction)
        Nothing -> fail ("missing fixture link: " ++ key)
    fixtureResult links key = case M.lookup key links of
        Just ("profit", amount) -> pure (CW.PeriodProfit amount)
        Just ("loss", amount) -> pure (CW.PeriodLoss amount)
        Just ("break-even", _) -> pure CW.PeriodBreakEven
        Just (direction, _) -> fail
            ("invalid period-result direction for " ++ key ++ ": " ++ direction)
        Nothing -> fail ("missing fixture link: " ++ key)
    fixtureBalance links key = case M.lookup key links of
        Just ("credit", amount) -> pure (CW.CreditBalance amount)
        Just ("debit", amount) -> pure (CW.DebitBalance amount)
        Just (direction, _) -> fail
            ("invalid balance direction for " ++ key ++ ": " ++ direction)
        Nothing -> fail ("missing fixture link: " ++ key)

leftErrors :: Either (NE.NonEmpty e) a -> Maybe (NE.NonEmpty e)
leftErrors (Left errors) = Just errors
leftErrors (Right _) = Nothing

testConsolidationWorksheet :: IO ()
testConsolidationWorksheet = do
    fixture <- loadMinimalConsolidationFixture
    validated <- case CW.validateConsolidationWorksheet fixture of
        Left errors -> fail ("valid consolidation fixture rejected: " ++ show errors)
        Right value -> pure value
    assertEqual "consolidation worksheet: source provenance retained"
        ["parent", "subsidiary"]
        [ CW._sourceId source
        | source <- NE.toList (CW.validatedSources validated)
        ]
    assertEqual "consolidation worksheet: adjustment provenance retained"
        [("nci-attribution", ["parent", "subsidiary"])]
        [ ( CW._adjustmentId adjustment
          , NE.toList (CW._adjustmentSourceIds adjustment)
          )
        | adjustment <- CW.validatedAdjustments validated
        ]
    let combined = CW.combinedWorksheet validated
    assertEqual "consolidation worksheet: combined fixture stays balanced"
        True (ECC.exactBalanced combined)
    assertEqual "consolidation worksheet: combined debit total"
        (200 :: MoneyDecimal) (EA.norm (EA.decL combined))
    assertEqual "consolidation worksheet: combined credit total"
        (200 :: MoneyDecimal) (EA.norm (EA.decR combined))
    assertEqual "consolidation worksheet: combination preserves all sequences"
        6 (L.length (EA.vals combined))

    let rawAdjustment =
            (10 EA..@ (Not :< Cash)) EA..+
            (5 EA..@ (Not :< Cash)) EA..+
            (15 EA..@ (Not :< Sales)) :: CheckedAlgM
        rawInput = CW.WorksheetInput (CW._worksheetSources fixture)
            [CW.WorksheetAdjustment "raw-three-posting"
                ("parent" NE.:| []) rawAdjustment]
            (CW._worksheetLinkage fixture)
    assertEqual "consolidation worksheet: accepts non-journal-shaped raw Alg"
        True
        (case CW.validateConsolidationWorksheet rawInput of
            Right _ -> True
            Left _  -> False)

    let sources = CW._worksheetSources fixture
        links = CW._worksheetLinkage fixture
        debitOnly = EC.journalFromSides
            [(Debit, Cash, 10 :: MoneyDecimal)] :: CheckedAlgM
        creditOnly = EC.journalFromSides
            [(Credit, Sales, 10 :: MoneyDecimal)] :: CheckedAlgM
        cancellingSet = debitOnly EA..+ creditOnly
        cancellingInput = CW.WorksheetInput sources
            [ CW.WorksheetAdjustment "bad-debit" ("parent" NE.:| []) debitOnly
            , CW.WorksheetAdjustment "bad-credit" ("parent" NE.:| []) creditOnly
            ] links
    assertEqual "consolidation worksheet: malformed set can balance in aggregate"
        True (ECC.exactBalanced cancellingSet)
    assertEqual "consolidation worksheet: atomic gate rejects both malformed adjustments"
        (Just
            ( CW.UnbalancedAdjustment "bad-debit" 10 0 NE.:|
              [CW.UnbalancedAdjustment "bad-credit" 0 10]
            ))
        (leftErrors (CW.validateConsolidationWorksheet cancellingInput))

    let forbidden = EC.journalFromSides
            [ (Debit, NetIncome, 10 :: MoneyDecimal)
            , (Credit, RetainedEarnings, 10)
            ] :: CheckedAlgM
        forbiddenInput = CW.WorksheetInput sources
            [CW.WorksheetAdjustment "forbidden" ("parent" NE.:| []) forbidden]
            links
    assertEqual "consolidation worksheet: context capability applies to raw Alg"
        (Just (CW.AdjustmentPostingNotAllowed "forbidden" NetIncome
            EngineGeneratedOnly NE.:| []))
        (leftErrors (CW.validateConsolidationWorksheet forbiddenInput))

    let provenanceInput = CW.WorksheetInput sources
            [CW.WorksheetAdjustment "unknown"
                ("ghost" NE.:| ["ghost"]) (mempty :: CheckedAlgM)] links
    assertEqual "consolidation worksheet: provenance rejects duplicate and unknown source"
        (Just
            ( CW.DuplicateAdjustmentSource "unknown" "ghost" NE.:|
              [ CW.UnknownAdjustmentSource "unknown" "ghost"
              , CW.EmptyAdjustment "unknown"
              ]
            ))
        (leftErrors (CW.validateConsolidationWorksheet provenanceInput))

    let mismatchedLinks = links
            { CW._statementOfChangesNetIncomeAttributableToOwners =
                CW.PeriodProfit 40 }
        mismatchedInput = fixture { CW._worksheetLinkage = mismatchedLinks }
    assertEqual "consolidation worksheet: P/L to S/S mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet mismatchedInput of
            Left errors -> CW.OwnersPeriodResultLinkMismatch
                (CW.PeriodProfit 50) (CW.PeriodProfit 40) `elem` NE.toList errors
            Right _ -> False)

    let attributionLinks = links
            { CW._profitOrLossNetIncome = CW.PeriodProfit 60 }
        attributionInput = fixture { CW._worksheetLinkage = attributionLinks }
    assertEqual "consolidation worksheet: total attribution mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet attributionInput of
            Left errors -> CW.NetIncomeAttributionMismatch 60 70
                `elem` NE.toList errors
            Right _ -> False)

    let retainedLinks = links
            { CW._retainedEarningsDividends = 11 }
        retainedInput = fixture { CW._worksheetLinkage = retainedLinks }
    assertEqual "consolidation worksheet: retained-earnings mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet retainedInput of
            Left errors -> CW.RetainedEarningsRollForwardMismatch 150 151
                `elem` NE.toList errors
            Right _ -> False)

    let balanceSheetLinks = links
            { CW._balanceSheetRetainedEarnings = CW.CreditBalance 139 }
        balanceSheetInput = fixture { CW._worksheetLinkage = balanceSheetLinks }
    assertEqual "consolidation worksheet: S/S to B/S mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet balanceSheetInput of
            Left errors -> CW.BalanceSheetRetainedEarningsMismatch
                (CW.CreditBalance 140) (CW.CreditBalance 139)
                `elem` NE.toList errors
            Right _ -> False)

    let nciLinks = links { CW._nonControllingInterestsDividends = 6 }
        nciInput = fixture { CW._worksheetLinkage = nciLinks }
    assertEqual "consolidation worksheet: NCI roll-forward mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet nciInput of
            Left errors -> CW.NonControllingInterestsRollForwardMismatch 50 51
                `elem` NE.toList errors
            Right _ -> False)

    let nciBalanceSheetLinks = links
            { CW._balanceSheetNonControllingInterests = CW.CreditBalance 44 }
        nciBalanceSheetInput = fixture
            { CW._worksheetLinkage = nciBalanceSheetLinks }
    assertEqual "consolidation worksheet: NCI S/S to B/S mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet nciBalanceSheetInput of
            Left errors -> CW.BalanceSheetNonControllingInterestsMismatch
                (CW.CreditBalance 45) (CW.CreditBalance 44)
                `elem` NE.toList errors
            Right _ -> False)

    let lossLinks = links
            { CW._profitOrLossNetIncome = CW.PeriodLoss 25
            , CW._profitOrLossNetIncomeAttributableToOwners = CW.PeriodLoss 20
            , CW._statementOfChangesNetIncomeAttributableToOwners =
                CW.PeriodLoss 20
            , CW._openingRetainedEarnings = CW.CreditBalance 100
            , CW._retainedEarningsDividends = 10
            , CW._statementOfChangesClosingRetainedEarnings =
                CW.CreditBalance 70
            , CW._balanceSheetRetainedEarnings = CW.CreditBalance 70
            , CW._openingNonControllingInterests = CW.CreditBalance 30
            , CW._nonControllingInterestsPeriodShare = CW.PeriodLoss 5
            , CW._nonControllingInterestsDividends = 5
            , CW._statementOfChangesClosingNonControllingInterests =
                CW.CreditBalance 20
            , CW._balanceSheetNonControllingInterests = CW.CreditBalance 20
            }
        lossInput = fixture { CW._worksheetLinkage = lossLinks }
    assertEqual "consolidation worksheet: loss roll-forwards preserve direction"
        True
        (case CW.validateConsolidationWorksheet lossInput of
            Right _ -> True
            Left _  -> False)

    let deficitLinks = links
            { CW._profitOrLossNetIncome = CW.PeriodLoss 50
            , CW._profitOrLossNetIncomeAttributableToOwners = CW.PeriodLoss 50
            , CW._statementOfChangesNetIncomeAttributableToOwners =
                CW.PeriodLoss 50
            , CW._openingRetainedEarnings = CW.CreditBalance 10
            , CW._retainedEarningsDividends = 0
            , CW._statementOfChangesClosingRetainedEarnings =
                CW.DebitBalance 40
            , CW._balanceSheetRetainedEarnings = CW.DebitBalance 40
            , CW._openingNonControllingInterests = CW.CreditBalance 0
            , CW._nonControllingInterestsPeriodShare = CW.PeriodBreakEven
            , CW._nonControllingInterestsDividends = 0
            , CW._statementOfChangesClosingNonControllingInterests =
                CW.CreditBalance 0
            , CW._balanceSheetNonControllingInterests = CW.CreditBalance 0
            }
        deficitInput = fixture { CW._worksheetLinkage = deficitLinks }
    assertEqual "consolidation worksheet: accumulated deficit is structural"
        True
        (case CW.validateConsolidationWorksheet deficitInput of
            Right _ -> True
            Left _  -> False)

    let invalidLinks = links
            { CW._openingRetainedEarnings = CW.CreditBalance (-1) }
        invalidInput = fixture { CW._worksheetLinkage = invalidLinks }
    assertEqual "consolidation worksheet: negative linkage amount rejected"
        (Just (CW.InvalidLinkAmount CW.OpeningRetainedEarnings (-1) NE.:| []))
        (leftErrors (CW.validateConsolidationWorksheet invalidInput))

    let wildcardAccount =
            (10 EA..@ (Not :< AccountTitle)) EA..+
            (10 EA..@ (Hat :< Cash)) :: CheckedAlgM
        wildcardInput = CW.WorksheetInput sources
            [CW.WorksheetAdjustment "wildcard" ("parent" NE.:| [])
                wildcardAccount] links
    assertEqual "consolidation worksheet: wildcard error list is total"
        (Just (CW.WildcardAdjustmentAccount "wildcard" NE.:| []))
        (leftErrors (CW.validateConsolidationWorksheet wildcardInput))

    let duplicateSources = case sources of
            source NE.:| rest -> source NE.:| (source : rest)
        duplicateAdjustments =
            [ CW.WorksheetAdjustment "same" ("parent" NE.:| [])
                rawAdjustment
            , CW.WorksheetAdjustment "same" ("parent" NE.:| [])
                rawAdjustment
            ]
        duplicateInput = CW.WorksheetInput duplicateSources
            duplicateAdjustments links
    assertEqual "consolidation worksheet: duplicate stable IDs rejected"
        True
        (case CW.validateConsolidationWorksheet duplicateInput of
            Left errors -> CW.DuplicateSourceId "parent" `elem` NE.toList errors
                && CW.DuplicateAdjustmentId "same" `elem` NE.toList errors
            Right _ -> False)

    let wildcardSource = CW.TrialBalanceSource "wild-source"
            (10 :@ (HatNot :< Cash) :: CheckedAlgM)
        wildcardSourceInput
            :: CW.WorksheetInput String String MoneyDecimal
        wildcardSourceInput = CW.WorksheetInput
            (wildcardSource NE.:| []) [] links
    assertEqual "consolidation worksheet: wildcard source side is total"
        (Just (CW.WildcardSourceSide "wild-source" NE.:| []))
        (leftErrors (CW.validateConsolidationWorksheet wildcardSourceInput))

    let wildcardSide =
            10 :@ (HatNot :< Cash) :: CheckedAlgM
        wildcardSideInput = CW.WorksheetInput sources
            [CW.WorksheetAdjustment "wild-side" ("parent" NE.:| [])
                wildcardSide] links
    assertEqual "consolidation worksheet: wildcard adjustment side is total"
        (Just (CW.WildcardAdjustmentSide "wild-side" NE.:| []))
        (leftErrors (CW.validateConsolidationWorksheet wildcardSideInput))

    let unbalancedSource = CW.TrialBalanceSource "unbalanced-source"
            (EC.journalFromSides
                [(Debit, Cash, 10 :: MoneyDecimal)] :: CheckedAlgM)
        unbalancedSourceInput
            :: CW.WorksheetInput String String MoneyDecimal
        unbalancedSourceInput = CW.WorksheetInput
            (unbalancedSource NE.:| []) [] links
    assertEqual "consolidation worksheet: unbalanced source rejected"
        (Just (CW.UnbalancedSourceTrialBalance
            "unbalanced-source" 10 0 NE.:| []))
        (leftErrors (CW.validateConsolidationWorksheet unbalancedSourceInput))

trialBalanceInput
    :: CheckedAlgM
    -> TB.TrialBalanceStage
    -> TB.TrialBalanceInput MoneyDecimal
trialBalanceInput alg stage = TB.TrialBalanceInput
    { TB._trialBalanceElement = alg
    , TB._trialBalanceStage = stage
    , TB._temporaryBalanceExplanations = M.empty
    , TB._reclassificationRules = []
    , TB._maturityEvidenceTitles = Set.empty
    }

testTrialBalanceValidation :: IO ()
testTrialBalanceValidation = do
    let reciprocalMismatch = EC.journalFromSides
            [ (Debit, BranchCurrentAccount, 40 :: MoneyDecimal)
            , (Credit, HeadOfficeCurrentAccount, 30)
            , (Credit, CapitalStock, 10)
            ] :: CheckedAlgM
        reciprocalInput = trialBalanceInput reciprocalMismatch TB.BeforeClosing
        expectedReciprocal = TB.ReciprocalMismatch
            (TB.DebitBalance 40) (TB.CreditBalance 30)
    assertEqual "trial balance: reciprocal mismatch independent of global balance"
        (Just (expectedReciprocal NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.strictTrialBalancePolicy reciprocalInput))
    assertEqual "trial balance: two-sided mismatch is never a standalone waiver"
        (Just (expectedReciprocal NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy reciprocalInput))
    let standaloneReciprocal = EC.journalFromSides
            [ (Debit, BranchCurrentAccount, 40 :: MoneyDecimal)
            , (Credit, CapitalStock, 40)
            ] :: CheckedAlgM
        standaloneInput = trialBalanceInput standaloneReciprocal TB.BeforeClosing
        expectedStandalone = TB.StandaloneReciprocalBalance
            BranchCurrentAccount (TB.DebitBalance 40)
    standalone <- case TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy standaloneInput of
        Left errors -> fail ("standalone reciprocal balance rejected: " ++ show errors)
        Right value -> pure value
    assertEqual "trial balance: standalone policy retains permitted finding"
        [expectedStandalone] (TB.validatedFindings standalone)

    let explainedSuspense = EC.journalFromSides
            [ (Debit, SuspensePayments, 10 :: MoneyDecimal)
            , (Credit, CapitalStock, 10)
            ] :: CheckedAlgM
        explanation = T.pack "invoice received after reporting date"
        explainedInput = (trialBalanceInput explainedSuspense TB.BeforeClosing)
            { TB._temporaryBalanceExplanations =
                M.singleton SuspensePayments explanation }
        expectedExplained = TB.ExplainedTemporaryBalance SuspensePayments
            (TB.DebitBalance 10) explanation
    explained <- case TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy explainedInput of
        Left errors -> fail ("explained suspense balance rejected: " ++ show errors)
        Right value -> pure value
    assertEqual "trial balance: explained temporary balance retained"
        [expectedExplained] (TB.validatedFindings explained)
    assertEqual "trial balance: policy can block explained temporary balance"
        (Just (expectedExplained NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.strictTrialBalancePolicy explainedInput))
    let unresolvedInput = trialBalanceInput explainedSuspense TB.BeforeClosing
    assertEqual "trial balance: unexplained temporary balance blocks"
        (Just (TB.UnresolvedTemporaryBalance SuspensePayments
            (TB.DebitBalance 10) NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy unresolvedInput))
    let blankInput = explainedInput
            { TB._temporaryBalanceExplanations =
                M.singleton SuspensePayments (T.pack "  ") }
    assertEqual "trial balance: blank explanation never opens the gate"
        (Just (TB.BlankTemporaryExplanation SuspensePayments
            (TB.DebitBalance 10) NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy blankInput))

    let closingResidual = EC.journalFromSides
            [ (Debit, CashOverShort, 10 :: MoneyDecimal)
            , (Credit, IncomeSummary, 10)
            ] :: CheckedAlgM
        closingInput = trialBalanceInput closingResidual TB.AfterClosing
    assertEqual "trial balance: closing devices must be zero after closing"
        (Just
            ( TB.ClosingDeviceResidual CashOverShort (TB.DebitBalance 10)
                NE.:|
              [TB.ClosingDeviceResidual IncomeSummary (TB.CreditBalance 10)]
            ))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy closingInput))
    let unclosedNominal = EC.journalFromSides
            [ (Debit, Cash, 10 :: MoneyDecimal)
            , (Credit, Sales, 10)
            ] :: CheckedAlgM
    assertEqual "trial balance: nominal balances must close after closing"
        (Just (TB.UnclosedNominalBalance Sales
            (TB.CreditBalance 10) NE.:| []))
        (leftErrors (TB.validateTrialBalance TB.standaloneTrialBalancePolicy
            (trialBalanceInput unclosedNominal TB.AfterClosing)))

    let derivedResidual = EC.journalFromSides
            [ (Debit, NetIncome, 10 :: MoneyDecimal)
            , (Credit, CapitalStock, 10)
            ] :: CheckedAlgM
    assertEqual "trial balance: derived coordinates are residuals after closing"
        (Just (TB.DerivedCoordinateResidual NetIncome
            (TB.DebitBalance 10) NE.:| []))
        (leftErrors (TB.validateTrialBalance TB.standaloneTrialBalancePolicy
            (trialBalanceInput derivedResidual TB.AfterClosing)))

    let abnormalDeposit = EC.journalFromSides
            [ (Debit, Cash, 10 :: MoneyDecimal)
            , (Credit, CurrentDeposits, 10)
            ] :: CheckedAlgM
        abnormalInput = trialBalanceInput abnormalDeposit TB.BeforeClosing
        oneRule = TB.SideReclassificationRule CurrentDeposits Credit
            (ShortTermLoansPayable NE.:| [])
        twoRules = TB.SideReclassificationRule CurrentDeposits Credit
            (BankOverdraft NE.:| [ShortTermLoansPayable])
    assertEqual "trial balance: unexplained abnormal side is explicit"
        (Just (TB.UnexplainedAbnormalBalance CurrentDeposits Debit
            (TB.CreditBalance 10) NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy abnormalInput))
    assertEqual "trial balance: unique reclassification is an instruction"
        (Just (TB.AbnormalBalanceWithReclassificationRule CurrentDeposits
            (TB.CreditBalance 10) ShortTermLoansPayable NE.:| []))
        (leftErrors (TB.validateTrialBalance TB.standaloneTrialBalancePolicy
            abnormalInput { TB._reclassificationRules = [oneRule] }))
    assertEqual "trial balance: ambiguous reclassification is never automatic"
        (Just (TB.AmbiguousReclassification CurrentDeposits
            (TB.CreditBalance 10)
            (BankOverdraft NE.:| [ShortTermLoansPayable]) NE.:| []))
        (leftErrors (TB.validateTrialBalance TB.standaloneTrialBalancePolicy
            abnormalInput { TB._reclassificationRules = [twoRules] }))

    let recordedTransfer = EC.journalFromSides
            [ (Debit, CurrentDeposits, 10 :: MoneyDecimal)
            , (Credit, ShortTermLoansPayable, 10)
            ] :: CheckedAlgM
        transferred = abnormalDeposit .+ recordedTransfer
        transferredInput = (trialBalanceInput transferred TB.BeforeClosing)
            { TB._reclassificationRules = [oneRule] }
    assertEqual "trial balance: recorded transfer clears abnormal finding"
        True
        (case TB.validateTrialBalance
                TB.standaloneTrialBalancePolicy transferredInput of
            Right _ -> True
            Left _ -> False)
    assertEqual "trial balance: validation never rewrites the admitted element"
        transferred
        (case TB.validateTrialBalance
                TB.standaloneTrialBalancePolicy transferredInput of
            Right value -> TB.validatedTrialBalance value
            Left _ -> mempty)
    assertEqual "trial balance: validated stage is retained"
        TB.BeforeClosing
        (case TB.validateTrialBalance
                TB.standaloneTrialBalancePolicy transferredInput of
            Right value -> TB.validatedStage value
            Left _ -> TB.AfterClosing)

    let invalidRule = TB.SideReclassificationRule CashOverShort Credit
            (MiscellaneousIncome NE.:| [])
        invalidRuleInput = (trialBalanceInput mempty TB.BeforeClosing)
            { TB._reclassificationRules = [invalidRule] }
    assertEqual "trial balance: inapplicable rules are explicit"
        (Just (TB.InapplicableReclassificationRule invalidRule NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy invalidRuleInput))
    let deadRule = TB.SideReclassificationRule CurrentDeposits Debit
            (Cash NE.:| [])
        deadRuleInput = (trialBalanceInput mempty TB.BeforeClosing)
            { TB._reclassificationRules = [deadRule] }
    assertEqual "trial balance: normal-side trigger is a dead rule"
        (Just (TB.InapplicableReclassificationRule deadRule NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy deadRuleInput))

    let maturityBalance = EC.journalFromSides
            [ (Debit, LoansReceivable, 25 :: MoneyDecimal)
            , (Credit, CapitalStock, 25)
            ] :: CheckedAlgM
        maturityRule = TB.MaturityEvidenceRequired LoansReceivable
        missingMaturity = (trialBalanceInput maturityBalance TB.BeforeClosing)
            { TB._reclassificationRules = [maturityRule] }
        suppliedMaturity = missingMaturity
            { TB._maturityEvidenceTitles = Set.singleton LoansReceivable }
    assertEqual "trial balance: maturity-sensitive title requires evidence"
        (Just (TB.MissingMaturityEvidence LoansReceivable NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy missingMaturity))
    assertEqual "trial balance: supplied maturity evidence clears finding"
        True
        (case TB.validateTrialBalance
                TB.standaloneTrialBalancePolicy suppliedMaturity of
            Right _ -> True
            Left _ -> False)

    let unbalanced = EC.journalFromSides
            [(Debit, Cash, 10 :: MoneyDecimal)] :: CheckedAlgM
        unbalancedInput = trialBalanceInput unbalanced TB.BeforeClosing
    assertEqual "trial balance: exact global imbalance blocks"
        (Just (TB.UnbalancedTrialBalance 10 0 NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy unbalancedInput))

    let wildcard = 10 :@ (HatNot :< Cash) :: CheckedAlgM
        wildcardInput = trialBalanceInput wildcard TB.BeforeClosing
    assertEqual "trial balance: wildcard side reports without whichSide crash"
        (Just (TB.WildcardTrialBalanceSide NE.:| []))
        (leftErrors (TB.validateTrialBalance
            TB.standaloneTrialBalancePolicy wildcardInput))

    let titlesFor role =
            [ title
            | title <- Registry.concreteAccountTitles
            , Just semantics <- [Registry.accountSemantics title]
            , role `elem` Registry.asemRoles semantics
            ]
    assertEqual "trial balance: reciprocal registry role is pinned"
        [BranchCurrentAccount, HeadOfficeCurrentAccount]
        (titlesFor ReciprocalAccount)
    assertEqual "trial balance: suspense registry role is pinned"
        [SuspensePayments, CashOverShort, SuspenseReceipts, SuspenseAccount]
        (titlesFor SuspenseOrClearingAccount)
    assertEqual "trial balance: closing-device registry role is pinned"
        [IncomeSummary] (titlesFor ClosingDevice)

testReportingPresentation :: IO ()
testReportingPresentation = do
    let reportingBalance = EC.journalFromSides
            [ (Debit, Cash, 100 :: MoneyDecimal)
            , (Debit, LoansReceivable, 30)
            , (Debit, Purchases, 40)
            , (Debit, BranchCurrentAccount, 10)
            , (Debit, IncomeTaxesRefundReceivable, 5)
            , (Credit, CapitalStock, 90)
            , (Credit, Sales, 70)
            , (Credit, AdvancesReceived, 15)
            , (Credit, HeadOfficeCurrentAccount, 10)
            ] :: CheckedAlgM
        maturityRule = TB.MaturityEvidenceRequired LoansReceivable
        reportingInput = (trialBalanceInput reportingBalance TB.BeforeClosing)
            { TB._reclassificationRules = [maturityRule]
            , TB._maturityEvidenceTitles = Set.singleton LoansReceivable
            }
        validated = case TB.validateTrialBalance
                TB.strictTrialBalancePolicy reportingInput of
            Right value -> value
            Left errors -> error ("reporting fixture did not validate: " ++ show errors)
        rationale = T.pack "material under the documented tax review"
        context scope = (RP.jcciSecondGradeContext scope)
            { RP._presentationAllocations =
                [RP.PresentationAllocation LoansReceivable 10 20
                    (T.pack "contract maturity schedule")]
            , RP._presentationRelabels =
                [RP.PresentationRelabel Purchases SalesCost
                    (T.pack "JCCI report cost-of-sales label")]
            , RP._materialityDecisions =
                [RP.MaterialityDecision IncomeTaxesRefundReceivable
                    RP.PresentSeparately rationale]
            , RP._subtotalDefinitions =
                [ RP.SubtotalDefinition RM.GrossProfitMetric [Sales] [SalesCost]
                    RP.RequireAllTitlesPresent
                , RP.SubtotalDefinition RM.OrdinaryProfitMetric [Sales] [SalesCost]
                    RP.RequireAllTitlesPresent
                ]
            }
        standalone = rightStatements (RP.present (context RP.Standalone) validated)
        combined = rightStatements (RP.present (context RP.Combined) validated)
        standaloneTitles = L.map RP._lineAccount (RP._statementLines standalone)
        combinedTitles = L.map RP._lineAccount (RP._statementLines combined)
    assertEqual "reporting: same validated TB changes with scope"
        True (RP._statementLines standalone /= RP._statementLines combined)
    assertEqual "reporting: standalone retains reciprocal lines"
        True (BranchCurrentAccount `elem` standaloneTitles
            && HeadOfficeCurrentAccount `elem` standaloneTitles)
    assertEqual "reporting: combined eliminates reciprocal lines"
        True (BranchCurrentAccount `notElem` combinedTitles
            && HeadOfficeCurrentAccount `notElem` combinedTitles
            && any isElimination (RP._presentationAudit combined))
    assertEqual "reporting: maturity evidence splits one title"
        [ (RP.CurrentAssetsSection, 10)
        , (RP.NoncurrentAssetsSection, 20)
        ]
        [ (RP._lineSection line, RP._lineAmount line)
        | line <- RP._statementLines standalone
        , RP._lineAccount line == LoansReceivable
        ]
    assertEqual "reporting: JCCI profile uses contract-liability label"
        [T.pack "契約負債"]
        [ RP._lineLabel line
        | line <- RP._statementLines standalone
        , RP._lineAccount line == AdvancesReceived
        ]
    assertEqual "reporting: Purchases is relabeled to SalesCost"
        True (Purchases `notElem` standaloneTitles && SalesCost `elem` standaloneTitles)
    assertEqual "reporting: GrossProfit is a subtotal, not a basis line"
        ( [ RP.StatementSubtotal RM.GrossProfitMetric
                (T.pack "売上総利益") (TB.CreditBalance 30)
          , RP.StatementSubtotal RM.OrdinaryProfitMetric
                (T.pack "経常利益") (TB.CreditBalance 30)
          ]
        , False
        )
        ( RP._statementSubtotals standalone
        , GrossProfit `elem` standaloneTitles
            || OrdinaryProfit `elem` standaloneTitles
        )
    assertEqual "reporting: materiality rationale survives in audit"
        True (RP.MaterialityApplied IncomeTaxesRefundReceivable
            RP.PresentSeparately (TB.DebitBalance 5) rationale
                `elem` RP._presentationAudit standalone)
    assertEqual "reporting: relabel never mutates validated bookkeeping coordinates"
        True (Purchases `elem` basesAccountTitles (TB.validatedTrialBalance validated)
            && SalesCost `notElem`
                basesAccountTitles (TB.validatedTrialBalance validated))

    let missingAllocation = (context RP.Standalone)
            { RP._presentationAllocations = [] }
    assertEqual "reporting: missing maturity evidence blocks presentation"
        True (case RP.present missingAllocation validated of
            Left issues -> RP.MissingPresentationAllocation LoansReceivable
                `elem` NE.toList issues
            Right _ -> False)

    let explainedSuspense = EC.journalFromSides
            [ (Debit, SuspensePayments, 10 :: MoneyDecimal)
            , (Credit, CapitalStock, 10)
            ] :: CheckedAlgM
        explainedInput = (trialBalanceInput explainedSuspense TB.BeforeClosing)
            { TB._temporaryBalanceExplanations = M.singleton SuspensePayments
                (T.pack "pending invoice") }
        explainedValidated = case TB.validateTrialBalance
                TB.standaloneTrialBalancePolicy explainedInput of
            Right value -> value
            Left errors -> error ("explained fixture rejected: " ++ show errors)
    assertEqual "reporting: stricter combined context re-gates retained finding"
        True (case RP.present (RP.jcciSecondGradeContext RP.Combined)
                explainedValidated of
            Left issues -> any isValidationBlock (NE.toList issues)
            Right _ -> False)

    let contraBalance = EC.journalFromSides
            [ (Debit, AccountsReceivable, 100 :: MoneyDecimal)
            , (Credit, AllowanceForDoubtfulAccounts, 10)
            , (Credit, CapitalStock, 90)
            ] :: CheckedAlgM
        contraValidated = validateFixture contraBalance
        separateContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._contraPresentationRules =
                [RP.PresentContraSeparately AllowanceForDoubtfulAccounts
                    (T.pack "show allowance as deduction")] }
        netContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._contraPresentationRules =
                [RP.NetContraAgainst AllowanceForDoubtfulAccounts
                    AccountsReceivable (T.pack "net receivables policy")] }
        separateLines = RP._statementLines
            (rightStatements (RP.present separateContext contraValidated))
        netLines = RP._statementLines
            (rightStatements (RP.present netContext contraValidated))
    assertEqual "reporting: contra policy supports separate presentation"
        True (any (\line -> RP._lineAccount line == AllowanceForDoubtfulAccounts
            && RP._lineIsDeduction line) separateLines)
    assertEqual "reporting: contra policy supports net presentation"
        [(Debit, 90)]
        [ (RP._lineSide line, RP._lineAmount line)
        | line <- netLines, RP._lineAccount line == AccountsReceivable
        ]

    let taxBalance = EC.journalFromSides
            [ (Debit, Cash, 85 :: MoneyDecimal)
            , (Debit, CorporateIncomeTaxes, 20)
            , (Credit, CapitalStock, 100)
            , (Credit, RefundOfIncomeTaxes, 5)
            ] :: CheckedAlgM
        taxValidated = validateFixture taxBalance
        netTaxContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._materialityDecisions =
                [RP.MaterialityDecision RefundOfIncomeTaxes
                    (RP.NetAgainst CorporateIncomeTaxes)
                    (T.pack "immaterial refund netted under tax policy")] }
        taxLines = RP._statementLines
            (rightStatements (RP.present netTaxContext taxValidated))
    assertEqual "reporting: materiality policy supports tax netting"
        (False, [(Debit, 15)])
        ( RefundOfIncomeTaxes `elem` L.map RP._lineAccount taxLines
        , [ (RP._lineSide line, RP._lineAmount line)
          | line <- taxLines, RP._lineAccount line == CorporateIncomeTaxes
          ]
        )
    let badAllocation current noncurrent evidence = (context RP.Standalone)
            { RP._presentationAllocations =
                [RP.PresentationAllocation LoansReceivable current noncurrent evidence] }
    assertEqual "reporting: blank allocation evidence blocks"
        True (hasPresentationIssue isBlankEvidence (RP.present
            (badAllocation 10 20 (T.pack "  ")) validated))
    assertEqual "reporting: negative allocation blocks"
        True (hasPresentationIssue isInvalidAllocation (RP.present
            (badAllocation (-1) 31 (T.pack "schedule")) validated))
    assertEqual "reporting: non-summing allocation blocks"
        True (hasPresentationIssue isInvalidAllocation (RP.present
            (badAllocation 10 19 (T.pack "schedule")) validated))
    let duplicateAllocation = (context RP.Standalone)
            { RP._presentationAllocations =
                [ RP.PresentationAllocation LoansReceivable 10 20
                    (T.pack "schedule A")
                , RP.PresentationAllocation LoansReceivable 10 20
                    (T.pack "schedule B")
                ] }
    assertEqual "reporting: duplicate allocation blocks"
        True (hasPresentationIssue isDuplicateAllocation
            (RP.present duplicateAllocation validated))
    let unexpectedAllocation = (context RP.Standalone)
            { RP._presentationAllocations =
                [ RP.PresentationAllocation LoansReceivable 10 20
                    (T.pack "contract maturity schedule")
                , RP.PresentationAllocation Cash 100 0 (T.pack "none")
                ] }
    assertEqual "reporting: unrequired allocation blocks"
        True (hasPresentationIssue isUnexpectedAllocation
            (RP.present unexpectedAllocation validated))

    let contraRequiredInput = (trialBalanceInput contraBalance TB.BeforeClosing)
            { TB._reclassificationRules =
                [TB.MaturityEvidenceRequired AccountsReceivable]
            , TB._maturityEvidenceTitles = Set.singleton AccountsReceivable
            }
        contraRequired = case TB.validateTrialBalance TB.strictTrialBalancePolicy
                contraRequiredInput of
            Right value -> value
            Left errors -> error ("required contra fixture rejected: " ++ show errors)
        netAllocatedContext = netContext
            { RP._presentationAllocations =
                [RP.PresentationAllocation AccountsReceivable 60 30
                    (T.pack "receivable maturity schedule")] }
    assertEqual "reporting: required net target can be allocated after netting"
        [(RP.CurrentAssetsSection, 60), (RP.NoncurrentAssetsSection, 30)]
        [ (RP._lineSection line, RP._lineAmount line)
        | line <- RP._statementLines
            (rightStatements (RP.present netAllocatedContext contraRequired))
        , RP._lineAccount line == AccountsReceivable
        ]
    let consumeRequired = (context RP.Standalone)
            { RP._materialityDecisions =
                [RP.MaterialityDecision LoansReceivable (RP.NetAgainst Cash)
                    (T.pack "must not erase maturity obligation")] }
    assertEqual "reporting: explicit maturity obligation cannot be consumed"
        True (hasPresentationIssue isConflictingInstruction
            (RP.present consumeRequired validated))

    let emptyValidated = validateFixture mempty
        emptyStatements = rightStatements (RP.present
            (RP.jcciSecondGradeContext RP.Combined) emptyValidated)
    assertEqual "reporting: empty combined TB has no fabricated elimination"
        ([], [])
        (RP._statementLines emptyStatements, RP._presentationAudit emptyStatements)
  where
    rightStatements (Right statements) = statements
    rightStatements (Left issues) = error ("presentation failed: " ++ show issues)
    validateFixture alg = case TB.validateTrialBalance TB.strictTrialBalancePolicy
            (trialBalanceInput alg TB.BeforeClosing) of
        Right value -> value
        Left errors -> error ("fixture did not validate: " ++ show errors)
    isElimination (RP.ReciprocalAccountsEliminated _ _) = True
    isElimination _ = False
    isValidationBlock (RP.ValidationFindingBlocks _) = True
    isValidationBlock _ = False
    hasPresentationIssue predicate (Left issues) = any predicate (NE.toList issues)
    hasPresentationIssue _ (Right _) = False
    isBlankEvidence (RP.BlankPresentationEvidence LoansReceivable) = True
    isBlankEvidence _ = False
    isInvalidAllocation (RP.InvalidPresentationAllocation LoansReceivable _ _ _) = True
    isInvalidAllocation _ = False
    isDuplicateAllocation (RP.DuplicatePresentationAllocation LoansReceivable) = True
    isDuplicateAllocation _ = False
    isUnexpectedAllocation (RP.UnexpectedPresentationAllocation Cash) = True
    isUnexpectedAllocation _ = False
    isConflictingInstruction
        (RP.ConflictingPresentationInstruction LoansReceivable) = True
    isConflictingInstruction _ = False
    basesAccountTitles alg =
        [ title | _ :< title <- EA.bases alg ]

testDerivedMetricsLand5 :: IO ()
testDerivedMetricsLand5 = do
    assertEqual "Land 5: legacy derived-coordinate ordinals and Binary bytes"
        [ (NetIncome, 49, T.pack "0031")
        , (GrossProfit, 54, T.pack "0036")
        , (OrdinaryProfit, 55, T.pack "0037")
        , (NetLoss, 64, T.pack "0040")
        , (IncomeSummary, 216, T.pack "00d8")
        ]
        [ (title, fromEnum title, accountSemanticsBinaryHex title)
        | title <- [NetIncome, GrossProfit, OrdinaryProfit, NetLoss, IncomeSummary]
        ]
    assertEqual "Land 5: exactly four legacy coordinates map to typed metrics"
        [ (NetIncome, RM.PeriodResultMetric)
        , (GrossProfit, RM.GrossProfitMetric)
        , (OrdinaryProfit, RM.OrdinaryProfitMetric)
        , (NetLoss, RM.PeriodResultMetric)
        ]
        [ (title, metric)
        | title <- Registry.concreteAccountTitles
        , Just metric <- [RM.metricForLegacyTitle title]
        ]

    let salesOnly = 100 .@ Not :< Sales :: CheckedAlgM
        afterLegacyBalancer = EAT.incomeSummaryAccount salesOnly
        hatSales = 25 .@ Hat :< Sales :: CheckedAlgM
    assertEqual "Land 5: period metric ignores an inserted legacy balancer"
        (Right (RM.PeriodProfit 100), Right (RM.PeriodProfit 100))
        ( RM.periodResultOfAlg salesOnly
        , RM.periodResultOfAlg afterLegacyBalancer
        )
    assertEqual "Land 5: Hat is interpreted through account side, not as a scalar sign"
        (Right (RM.PeriodLoss 25)) (RM.periodResultOfAlg hatSales)
    assertEqual "Land 5: empty nominal basis is break-even"
        (Right RM.PeriodBreakEven :: Either RM.MetricError (RM.PeriodResult MoneyDecimal))
        (RM.periodResultOfAlg (mempty :: CheckedAlgM))
    assertEqual "Land 5: raw metric boundary rejects wildcard sides"
        (Left (RM.WildcardMetricSide Sales))
        (RM.periodResultOfAlg (10 .@ HatNot :< Sales :: CheckedAlgM))

    let ordinaryLedger = EC.journalFromSides
            [ (Debit, Cash, 100 :: MoneyDecimal)
            , (Credit, Sales, 100)
            ] :: CheckedAlgM
        ordinaryValidated = validateBefore ordinaryLedger
    assertEqual "Land 5: validated before-closing TB derives one period-result identity"
        (Right (RM.PeriodProfit 100))
        (RM.periodResultOf ordinaryValidated)

    let legacyLedger = EC.journalFromSides
            [ (Debit, NetIncome, 10 :: MoneyDecimal)
            , (Credit, CapitalStock, 10)
            ] :: CheckedAlgM
        legacyValidated = validateBefore legacyLedger
    assertEqual "Land 5: typed metric rejects a residual legacy coordinate"
        (Left (RM.ResidualDerivedCoordinate NetIncome))
        (RM.periodResultOf legacyValidated)
    assertEqual "Land 5: legacy intermediate and presentation paths are explicit alternatives"
        True (case RP.present
                (RP.jcciSecondGradeContext RP.Standalone) legacyValidated of
            Left issues -> RP.UnpresentableBalance NetIncome
                (TB.DebitBalance 10) `elem` NE.toList issues
            Right _ -> False)

    let emptyValidated = validateBefore (mempty :: CheckedAlgM)
        subtotal = RP.SubtotalDefinition RM.GrossProfitMetric
            [Sales] [SalesCost] RP.TreatAbsentAsZero
        duplicateContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._subtotalDefinitions = [subtotal, subtotal] }
    assertEqual "Land 5: duplicate metric identity blocks presentation"
        True (case RP.present duplicateContext emptyValidated of
            Left issues -> RP.DuplicateMetricIdentity RM.GrossProfitMetric
                `elem` NE.toList issues
            Right _ -> False)
    let absentAsZeroContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._subtotalDefinitions = [subtotal] }
    assertEqual "Land 5: canonical subtotal may treat unposted titles as zero"
        [RP.StatementSubtotal RM.GrossProfitMetric
            (T.pack "売上総損益") TB.NoBalance]
        (RP._statementSubtotals (case RP.present absentAsZeroContext emptyValidated of
            Right statements -> statements
            Left issues -> error ("absent-as-zero subtotal rejected: " ++ show issues)))

    let relabelledLedger = EC.journalFromSides
            [ (Debit, Purchases, 30 :: MoneyDecimal)
            , (Credit, Sales, 30)
            ] :: CheckedAlgM
        relabelledValidated = validateBefore relabelledLedger
        removedTitleContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._presentationRelabels =
                [RP.PresentationRelabel Purchases SalesCost (T.pack "policy")]
            , RP._subtotalDefinitions =
                [RP.SubtotalDefinition RM.GrossProfitMetric
                    [Sales] [Purchases] RP.TreatAbsentAsZero]
            }
    assertEqual "Land 5: absent-as-zero does not hide a relabelled non-zero title"
        True (case RP.present removedTitleContext relabelledValidated of
            Left issues -> RP.InvalidSubtotalDefinition RM.GrossProfitMetric
                `elem` NE.toList issues
            Right _ -> False)

    let Just customId = RM.mkMetricId (T.pack "ebitda-adjusted")
        customDefinition = RP.SubtotalDefinition (RM.CustomMetric customId)
            [Sales] [] RP.TreatAbsentAsZero
        unlabelledContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._subtotalDefinitions = [customDefinition] }
    assertEqual "Land 5: custom metric identity requires separate labels"
        True (case RP.present unlabelledContext emptyValidated of
            Left issues -> RP.UnlabelledCustomMetric customId
                `elem` NE.toList issues
            Right _ -> False)
    let labelledContext = unlabelledContext
            { RP._presentationProfile = RP.CanonicalEnglish
            , RP._customMetricLabels =
                [RP.CustomMetricLabel customId
                    (T.pack "調整後EBITDA") (T.pack "Adjusted EBITDA")]
            }
    assertEqual "Land 5: custom metric identity and profile label are separate"
        [RP.StatementSubtotal (RM.CustomMetric customId)
            (T.pack "Adjusted EBITDA") TB.NoBalance]
        (RP._statementSubtotals (case RP.present labelledContext emptyValidated of
            Right statements -> statements
            Left issues -> error ("labelled custom metric rejected: " ++ show issues)))
    let duplicateLabelContext = labelledContext
            { RP._customMetricLabels = RP._customMetricLabels labelledContext
                ++ RP._customMetricLabels labelledContext
            }
    assertEqual "Land 5: standalone label lookup rejects duplicate identities"
        Nothing
        (RP.metricLabel duplicateLabelContext (RM.CustomMetric customId)
            TB.NoBalance)
  where
    validateBefore alg = case TB.validateTrialBalance
            TB.strictTrialBalancePolicy (trialBalanceInput alg TB.BeforeClosing) of
        Right value -> value
        Left errors -> error ("Land 5 fixture did not validate: " ++ show errors)

testPostingCapabilityGate :: IO ()
testPostingCapabilityGate = do
    let contexts =
            [ ECC.OrdinaryJournal
            , ECC.ClosingProcess
            , ECC.ConsolidationWorksheet
            , ECC.EngineComputation
            ]
        capabilities =
            [ OrdinaryPosting
            , ClosingOnly
            , ConsolidationOnly
            , EngineGeneratedOnly
            , NotPostable
            ]
        allowed context capability = (context, capability) `elem`
            [ (ECC.OrdinaryJournal, OrdinaryPosting)
            , (ECC.ClosingProcess, OrdinaryPosting)
            , (ECC.ClosingProcess, ClosingOnly)
            , (ECC.ConsolidationWorksheet, OrdinaryPosting)
            , (ECC.ConsolidationWorksheet, ConsolidationOnly)
            , (ECC.EngineComputation, OrdinaryPosting)
            , (ECC.EngineComputation, EngineGeneratedOnly)
            ]
    assertEqual "posting gate: closed context/capability matrix"
        [ (context, capability, allowed context capability)
        | context <- contexts
        , capability <- capabilities
        ]
        [ (context, capability, ECC.postingAllowedIn context capability)
        | context <- contexts
        , capability <- capabilities
        ]
    assertEqual "posting gate: all 232 titles follow the closed matrix"
        [ (context, title, ECC.postingAllowedIn context capability)
        | context <- contexts
        , title <- Registry.concreteAccountTitles
        , Just semantics <- [Registry.accountSemantics title]
        , let capability = Registry.asemPostingCapability semantics
        ]
        [ (context, title, accepted context title)
        | context <- contexts
        , title <- Registry.concreteAccountTitles
        ]
    assertEqual "posting gate: derived profit coordinates stay engine-only"
        (replicate 4 (Just EngineGeneratedOnly))
        [ Registry.asemPostingCapability <$> Registry.accountSemantics title
        | title <- [GrossProfit, OrdinaryProfit, NetIncome, NetLoss]
        ]
    assertEqual "posting gate: consolidation-only set is closed"
        [ NonControllingInterests
        , NetIncomeAttributableToNCI
        , NetLossAttributableToNCI
        ]
        [ title
        | title <- Registry.concreteAccountTitles
        , Just semantics <- [Registry.accountSemantics title]
        , Registry.asemPostingCapability semantics == ConsolidationOnly
        ]

    assertEqual "posting gate: ordinary wrapper rejects engine-generated result"
        (Left (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
            ECC.OrdinaryJournal NE.:| []))
        (checkedEntryM
            [ (Debit, NetIncome, 10)
            , (Credit, RetainedEarnings, 10)
            ])

    assertEqual "posting gate: closing admits IncomeSummary"
        True
        (case ECC.checkedEntryIn ECC.ClosingProcess
            [ (Debit, Sales, 10 :: MoneyDecimal)
            , (Credit, IncomeSummary, 10)
            ] of
            Right _ -> True
            Left _  -> False)
    assertEqual "posting gate: closing rejects engine-generated result"
        True
        (case ECC.checkedEntryIn ECC.ClosingProcess
            [ (Debit, NetIncome, 10 :: MoneyDecimal)
            , (Credit, RetainedEarnings, 10)
            ] of
            Left (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
                    ECC.ClosingProcess NE.:| []) -> True
            _ -> False)

    assertEqual "posting gate: consolidation admits NCI attribution"
        True
        (case ECC.checkedEntryIn ECC.ConsolidationWorksheet
            [ (Debit, NetIncomeAttributableToNCI, 10 :: MoneyDecimal)
            , (Credit, NonControllingInterests, 10)
            ] of
            Right _ -> True
            Left _  -> False)
    assertEqual "posting gate: ordinary journal rejects NCI equity"
        True
        (case ECC.checkedEntry
            [ (Debit, Cash, 10 :: MoneyDecimal)
            , (Credit, NonControllingInterests, 10)
            ] of
            Left (ECC.PostingNotAllowed 1 NonControllingInterests
                    ConsolidationOnly ECC.OrdinaryJournal NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: engine admits period result"
        True
        (case ECC.checkedEntryIn ECC.EngineComputation
            [ (Debit, NetIncome, 10 :: MoneyDecimal)
            , (Credit, RetainedEarnings, 10)
            ] of
            Right _ -> True
            Left _  -> False)

    assertEqual "posting gate: text path uses ordinary context"
        True
        (case ECC.checkedEntryText
            [ (T.pack "debit", T.pack "NetIncome", 10 :: MoneyDecimal)
            , (T.pack "credit", T.pack "RetainedEarnings", 10)
            ] of
            Left (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
                    ECC.OrdinaryJournal NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: unknown account does not create false imbalance"
        True
        (case ECC.checkedEntryText
            [ (T.pack "debit", T.pack "UnknownAccount_X", 10 :: MoneyDecimal)
            , (T.pack "credit", T.pack "Cash", 10)
            ] of
            Left (ECC.EntryParse 0 _ NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: consolidation text path admits NCI loss"
        True
        (case ECC.checkedEntryTextIn ECC.ConsolidationWorksheet
            [ (T.pack "debit", T.pack "NonControllingInterests", 10 :: MoneyDecimal)
            , (T.pack "credit", T.pack "NetLossAttributableToNCI", 10)
            ] of
            Right _ -> True
            Left _  -> False)
    assertEqual "posting gate: journal error retains txid"
        True
        (case checkedJournalM
            [ (7,
                [ (Debit, IncomeSummary, 10)
                , (Credit, RetainedEarnings, 10)
                ])
            ] of
            Left (ECC.EntryErrors 7
                    (ECC.PostingNotAllowed 0 IncomeSummary ClosingOnly
                        ECC.OrdinaryJournal NE.:| []) NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: certification rejects known disallowed title"
        True
        (case ECC.certifyJournalText
            [ (9 :: Int,
                [ (T.pack "debit", T.pack "NetIncome", 10 :: MoneyDecimal)
                , (T.pack "credit", T.pack "RetainedEarnings", 10)
                ])
            ] of
            ECC.Rejected
                (ECC.EntryErrors 9
                    (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
                        ECC.OrdinaryJournal NE.:| []) NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: disallowed known title outranks unresolved title"
        True
        (case ECC.certifyJournalText
            [ (11 :: Int,
                [ (T.pack "debit", T.pack "NetIncome", 10 :: MoneyDecimal)
                , (T.pack "credit", T.pack "UnknownAccount_X", 10)
                ])
            ] of
            ECC.Rejected
                (ECC.EntryErrors 11
                    (ECC.PostingNotAllowed 0 NetIncome EngineGeneratedOnly
                        ECC.OrdinaryJournal NE.:| []) NE.:| []) -> True
            _ -> False)
    assertEqual "posting gate: certification honours closing context"
        True
        (case ECC.certifyJournalTextIn ECC.ClosingProcess
            [ (10 :: Int,
                [ (T.pack "debit", T.pack "Sales", 10 :: MoneyDecimal)
                , (T.pack "credit", T.pack "IncomeSummary", 10)
                ])
            ] of
            ECC.FullyResolved _ -> True
            _                   -> False)
    assertEqual "posting gate: certification honours engine context"
        True
        (case ECC.certifyJournalTextIn ECC.EngineComputation
            [ (12 :: Int,
                [ (T.pack "debit", T.pack "NetLoss", 10 :: MoneyDecimal)
                , (T.pack "credit", T.pack "RetainedEarnings", 10)
                ])
            ] of
            ECC.FullyResolved _ -> True
            _                   -> False)
  where
    accepted context title =
        case ECC.checkedEntryIn context
            [ (Debit, title, 1 :: MoneyDecimal)
            , (Credit, Cash, 1)
            ] of
            Right _ -> True
            Left _  -> False

checkedConvertProperties :: IO ()
checkedConvertProperties = do
    testPostingCapabilityGate
    testConsolidationWorksheet
    testTrialBalanceValidation
    testReportingPresentation
    testDerivedMetricsLand5

    quickProp "convert-checked: certify known accounts matches checkedJournal" $
        prop_certifyKnownAccountsMatchesCheckedJournal

    quickProp "convert-checked: unresolved vocabulary preserves balance" $
        prop_certifyUnknownAccountPreservesBalance

    quickProp "convert-checked: imbalance precedes unresolved vocabulary" $
        prop_certifyImbalancePrecedesUnknownAccount

    quickProp "convert-checked: certify duplicate txid always rejected" $
        prop_certifyDuplicateTxIdAlwaysRejected

    quickProp "convert-checked: balanced unresolved totals match input" $
        prop_certifyBalancedUnresolvedTotals

    quickProp "convert-checked: checkedEntry accepts iff checked predicate" $
        forAll genCheckedEntryRows $ \rows ->
            let expected = checkedEntryAcceptsSpec rows
                actual = case checkedEntryM rows of
                    Right _ -> True
                    Left _  -> False
            in actual == expected

    quickProp "convert-checked: checkedEntry equals journalFromSides on accept" $
        forAll genCheckedEntryRows $ \rows ->
            case checkedEntryM rows of
                Right alg -> alg == (EC.journalFromSides rows :: CheckedAlgM)
                Left _    -> True

    quickProp "convert-checked: accepted entries form exact-balanced submonoid" $
        forAll genAcceptedEntryRows $ \rows1 ->
        forAll genAcceptedEntryRows $ \rows2 ->
            case (checkedEntryM rows1, checkedEntryM rows2) of
                (Right alg1, Right alg2) -> ECC.exactBalanced (alg1 .+ alg2)
                _                        -> False

    quickProp "convert-checked: checkedJournal duplicate txid only DuplicateTxId" $
        forAll genAcceptedEntryRows $ \rows1 ->
        forAll genAcceptedEntryRows $ \rows2 ->
            case checkedJournalM [(1, rows1), (1, rows2)] of
                Left errs -> NE.toList errs == [ECC.DuplicateTxId 1]
                Right _   -> False

    quickProp "convert-checked: reconcileSources coverage and amount checks" $
        forAll genPositiveAmountMD $ \amount ->
            let entry amt = [(Debit, Cash, amt), (Credit, Sales, amt)]
                shifted = amount + 1
                journalResult = checkedJournalM [(1, entry amount)]
                unknownResult = checkedJournalM [(1, entry amount), (2, entry 5)]
            in case (journalResult, unknownResult) of
                (Right journal, Right journalWithUnknown) ->
                    ECC.reconcileSources [(1, amount)] journal == []
                    && ECC.reconcileSources [(1, amount), (2, 5)] journal
                        == [ECC.MissingSource 2]
                    && ECC.reconcileSources [(1, amount)] journalWithUnknown
                        == [ECC.UnknownSource 2]
                    && ECC.reconcileSources [(1, shifted)] journal
                        == [ECC.AmountMismatch 1 shifted amount]
                _ -> False

axiomProperties :: IO ()
axiomProperties = do
    -- Definition 6 axioms (Double; semantic equality via exact per-base nets)
    quickProp "axiom: Hat involution (x^^ = x)" $
        forAll genAlgD $ \x -> netByBase ((.^) ((.^) x)) == netByBase x
    quickProp "axiom: scalar on singleton (a*(v:@b) = (a*v):@b)" $
        forAll genNNDouble $ \a -> forAll genNNDouble $ \v -> forAll genBase $ \b ->
            netByBase (a .* (v .@ b)) == netByBase (((a * v) .@ b) :: TestAlg)
    quickProp "axiom: scalar distributes over (.+)" $
        forAll genNNDouble $ \a -> forAll genAlgD $ \x -> forAll genAlgD $ \y ->
            netByBase (a .* (x .+ y)) == netByBase ((a .* x) .+ (a .* y))
    quickProp "axiom: norm additivity (norm(x+y) = norm x + norm y)" $
        forAll genAlgD $ \x -> forAll genAlgD $ \y ->
            epsEq (norm (x .+ y)) (norm x + norm y)
    quickProp "axiom: norm homogeneity (norm(a*x) = a*norm x, a>=0)" $
        forAll genNNDouble $ \a -> forAll genAlgD $ \x ->
            epsEq (norm (a .* x)) (a * norm x)
    -- derived lemmas
    quickProp "lemma: bar idempotent (bar(bar x) = bar x)" $
        forAll genAlgD $ \x -> netByBase (bar (bar x)) == netByBase (bar x)
    quickProp "lemma: zero identity (x .+ Zero = x)" $
        forAll genAlgD $ \x -> netByBase (x .+ EA.Zero) == netByBase x
    quickProp "lemma: (.+) associative" $
        forAll genAlgD $ \x -> forAll genAlgD $ \y -> forAll genAlgD $ \z ->
            netByBase ((x .+ y) .+ z) == netByBase (x .+ (y .+ z))
    -- regression: union must not relabel a value onto a zero posting's base
    -- (the 0.4.1.1 bug; raw (:@) so zero-valued singletons are exercised)
    quickProp "regression: union preserves per-base net (zero-base bug)" $
        forAll genNNDouble $ \v1 -> forAll genBase $ \b1 ->
        forAll genNNDouble $ \v2 -> forAll genBase $ \b2 ->
            let s1 = v1 :@ b1 :: TestAlg
                s2 = v2 :@ b2 :: TestAlg
            in netByBase (s1 .+ s2)
                 == M.unionWith (+) (netByBase s1) (netByBase s2)
    -- construction-order independence for the exact value type (MoneyDecimal)
    quickProp "MoneyDecimal: fromList per-base net is construction-order independent" $
        forAll (listOf ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)) $ \ps ->
            let singles = [ v :@ b | (v, b) <- ps ] :: [NNAlg]
                viaList  = EA.fromList singles
                viaFoldr = foldr   (.+) EA.Zero singles
                viaFoldl = L.foldl' (.+) EA.Zero singles
            in netByBase viaList == netByBase viaFoldr
               && netByBase viaFoldr == netByBase viaFoldl
    -- mapBasePart (Phase 3): identity + norm preservation (no value lost on collision)
    quickProp "mapBasePart id preserves per-base net (MoneyDecimal)" $
        forAll genAlgN $ \x -> netByBase (EA.mapBasePart id x :: NNAlg) == netByBase x
    quickProp "mapBasePart preserves norm under base collapse (MoneyDecimal)" $
        forAll genAlgN $ \x -> norm (EA.mapBasePart (const Amount) x :: NNAlg) == norm x
    -- S-4: functoriality of the base-relabel map pi_kappa (Prop 2.8(4)):
    --   mapBasePart (kappa' . kappa) x  ~=_pi  mapBasePart kappa' (mapBasePart kappa x)
    -- The two kappa are non-identity, non-injective relabelers on CountUnit so the
    -- composite collapses bases (Yen -> Dollar -> Amount), exercising the value
    -- merge on both sides. There is no dedicated ~=_pi comparator in this suite;
    -- we use 'netByBase' (per-base signed net), which is the same bar/order-robust
    -- observational equality the other mapBasePart / axiom properties use -- i.e.
    -- "equal after bar, compared per base". The 'kappa' relabels the BasePart
    -- (= CountUnit here), with mapBasePart re-merging colliding sides, so this is
    -- exactly the bar-then-map equivalence the audit note specifies.
    quickProp "S-4: mapBasePart is functorial (pi_{k'.k} ~=_pi pi_k' . pi_k, MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let kappa, kappa' :: CountUnit -> CountUnit
                kappa  u = if u == Yen    then Dollar else u   -- Yen    -> Dollar
                kappa' u = if u == Dollar then Amount else u   -- Dollar -> Amount
                lhs = EA.mapBasePart (kappa' . kappa) x                  :: NNAlg
                inner = EA.mapBasePart kappa x                           :: NNAlg
                rhs = EA.mapBasePart kappa' inner                        :: NNAlg
            in netByBase lhs == netByBase rhs
    -- netPairMapBy (ν_κ pair read-out): three properties from the
    -- easp-2026-06-11-netpairmapby handoff.
    -- (a) signed-diff consistency: balanceMapBy == n - h of the pair.
    --     n - h can be negative, so this is checked on the SIGNED value type
    --     (Double); a non-negative-only type would break the n-h component.
    quickProp "netPairMapBy: balanceMapBy x == n - h of netPairMapBy x (Double, signed)" $
        forAll genAlgD $ \x ->
            let bm = EA.balanceMapBy Just x                            :: M.Map CountUnit Double
                np = EA.netPairMapBy Just x                            :: M.Map CountUnit (Double, Double)
                diff = fmap (\(n, h) -> n - h) np
            -- balanceMapBy keeps zero-net keys; netPairMapBy drops them.
            -- Compare on the union: a key absent from one side reads as 0.
            in all (\k -> epsEq (M.findWithDefault 0 k bm)
                                (M.findWithDefault 0 k diff))
                   (M.keys bm ++ M.keys diff)
    -- (b) both pair components are non-negative (value-domain regularity).
    --     Exact value type so the >= 0 check has no tolerance ambiguity.
    quickProp "netPairMapBy: both components non-negative (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            all (\(n, h) -> n >= 0 && h >= 0)
                (M.elems (EA.netPairMapBy Just x :: M.Map CountUnit (MoneyDecimal, MoneyDecimal)))
    -- (c) ~=_pi invariance: like the S-4 / netByBase observational equality,
    --     the pair read-out is construction-order independent (bar-then-net is
    --     robust to seq order and reassociation). Exact MoneyDecimal.
    quickProp "netPairMapBy: ~=_pi invariant (construction-order independent, MoneyDecimal)" $
        forAll (listOf ((,) <$> (realToFrac <$> genNNDouble) <*> genBase)) $ \ps ->
            let singles  = [ v :@ b | (v, b) <- ps ] :: [NNAlg]
                viaList  = EA.netPairMapBy Just (EA.fromList singles)
                viaFoldr = EA.netPairMapBy Just (foldr   (.+) EA.Zero singles)
                viaFoldl = EA.netPairMapBy Just (L.foldl' (.+) EA.Zero singles)
            in viaList == (viaFoldr :: M.Map CountUnit (MoneyDecimal, MoneyDecimal))
               && viaFoldr == viaFoldl

-- ================================================================
-- Journal-algebra axiom properties (Phase 1.5)
-- ================================================================

type NNJournal = EJ.Journal String MoneyDecimal (HatBase CountUnit)

genNote :: Gen String
genNote = elements ["a", "b", "c"]

genPosNN :: Gen MoneyDecimal               -- strictly positive (avoids zero-note drop)
genPosNN = (\d -> realToFrac (1 + d)) <$> genNNDouble

genJournalN :: Gen NNJournal
genJournalN = sized $ \n -> do
    k  <- choose (0, min 30 n)
    ps <- vectorOf k ((,,) <$> genPosNN <*> genBase <*> genNote)
    pure (EJ.fromList [ (v :@ b) .| nt | (v, b, nt) <- ps ])

-- per-(note, base) signed net; exact (Rational)
netJournal :: NNJournal -> M.Map (String, CountUnit) Rational
netJournal j = M.fromList
    [ ((nt, u), r)
    | (nt, alg) <- HM.toList (EJ.toMap j)
    , (u, r)    <- M.toList (netByBase alg) ]

journalProperties :: IO ()
journalProperties = do
    quickProp "journal: norm additivity (norm(j1.+j2) = norm j1 + norm j2, MoneyDecimal)" $
        forAll genJournalN $ \j1 -> forAll genJournalN $ \j2 ->
            norm (j1 .+ j2) == norm j1 + norm j2
    quickProp "journal: Hat preserves the note set" $
        forAll genJournalN $ \j ->
            L.sort (HM.keys (EJ.toMap ((.^) j))) == L.sort (HM.keys (EJ.toMap j))
    quickProp "journal: fromList per-(note,base) net is construction-order independent (MoneyDecimal)" $
        forAll (listOf ((,,) <$> genPosNN <*> genBase <*> genNote)) $ \ps ->
            let js = [ (v :@ b) .| nt | (v, b, nt) <- ps ] :: [NNJournal]
            in netJournal (EJ.fromList js) == netJournal (foldr (.+) mempty js)

-- ================================================================
-- Quotient decomposition properties (Phase 1, feat/quotient-decomposition)
--
-- Encodes the dec_κ / π_κ axioms of the scaling formalization
-- (agent-notes/drafts/scaling-formalization.md §2, §7) as QuickCheck
-- properties, plus fixed sentinels for the side-sensitive non-commutation
-- cases that MUST NOT silently start commuting (they encode a semantic
-- choice, not a bug).
-- ================================================================

-- proper classifier: factors through the base part (never sees Hat/Not)
properKf :: HatBase CountUnit -> Maybe CountUnit
properKf (_ :< u) = Just u

-- partial proper classifier: Yen entries fall into the residual
partialKf :: HatBase CountUnit -> Maybe CountUnit
partialKf (_ :< Yen) = Nothing
partialKf (_ :< u)   = Just u

-- side-sensitive classifier: sees the Hat/Not state (like decP/decM)
sideKf :: HatBase CountUnit -> Maybe Bool
sideKf b = Just (isHat b)

-- residual of a partial classifier (reference implementation via filter)
residualOf :: (HatBase CountUnit -> Maybe CountUnit) -> NNAlg -> NNAlg
residualOf kf = EA.filter (\s -> s /= EA.Zero && kf (EA._hatBase s) == Nothing)

-- per-base nets with exact-zero entries dropped (bar drops zero-net bases,
-- so commutation properties are compared modulo zero nets)
nonZeroNet :: NNAlg -> M.Map CountUnit Rational
nonZeroNet = M.filter (/= 0) . netByBase

quotientProperties :: IO ()
quotientProperties = do
    -- reconstruction: Σ_k x_k (+ residual) = x  (formalization Prop 2.3)
    quickProp "decBy: reconstruction, total classifier (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            netByBase (mconcat (M.elems (EA.decBy properKf x))) == netByBase x
    quickProp "decBy: reconstruction with residual, partial classifier" $
        forAll genAlgN $ \x ->
            netByBase (mconcat (M.elems (EA.decBy partialKf x)) .+ residualOf partialKf x)
                == netByBase x
    -- norm additivity over classes (formalization Prop 2.4(1))
    quickProp "decBy: norm additivity over classes + residual (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            norm x == L.foldl' (+) 0 (L.map norm (M.elems (EA.decBy partialKf x)))
                      + norm (residualOf partialKf x)
    -- proper classifier commutes with bar componentwise (Prop 2.4(4))
    quickProp "decBy: bar commutes componentwise (proper classifier)" $
        forAll genAlgN $ \x ->
            M.filter (not . M.null) (M.map nonZeroNet (EA.decBy properKf (bar x)))
                == M.filter (not . M.null) (M.map (nonZeroNet . bar) (EA.decBy properKf x))
    -- decBy equals the naive per-class filter loop (semantics check)
    quickProp "decBy: equals naive per-class filter (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let d = EA.decBy properKf x
                naive k = EA.filter
                    (\s -> s /= EA.Zero && properKf (EA._hatBase s) == Just k) x
            in all (\(k, alg) -> netByBase alg == netByBase (naive k)) (M.toList d)
    -- postFromNetBy equals an independent per-key projNetNorm pipeline
    quickProp "postFromNetBy: equals per-key projNetNorm reference (MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let kf b = if isHat b then Just (unitOf b) else Nothing
                unitOf (_ :< u) = u
                post u v = v .@ (Not :< u) :: NNAlg
                viaApi = EA.postFromNetBy kf post x
                viaRef = mconcat
                    [ post u s
                    | u <- [Yen, Dollar, Amount]
                    , let s = EA.projNetNorm [Hat :< u] (bar x)
                    , s /= 0 ]
            in netByBase viaApi == netByBase viaRef
    -- decTo: flatten reconstructs and norm is preserved (total classifier)
    quickProp "decTo: toAlg . decTo reconstructs (total classifier, MoneyDecimal)" $
        forAll genAlgN $ \x ->
            let j = EJ.decTo (\(_ :< u) -> Just (show u)) x
                    :: EJ.Journal String MoneyDecimal (HatBase CountUnit)
            in netByBase (EJ.toAlg j) == netByBase x && norm j == norm x
    -- sentinel: side-sensitive classifier does NOT commute with bar
    -- (decP/decM-style split; x = v:@Not:<Yen .+ v:@Hat:<Yen nets to zero
    --  globally but each side survives within its own class)
    let xCancel = (5 .@ (Not :< Yen)) .+ (5 .@ (Hat :< Yen)) :: NNAlg
        lhs = M.filter (not . EA.isZero) (M.map bar (EA.decBy sideKf xCancel))
        rhs = EA.decBy sideKf (bar xCancel)
    assertEqual "sentinel: side-sensitive decBy does not commute with bar"
        True (M.keys lhs /= M.keys rhs)
    -- sentinel: whichSide-style classifier is also side-sensitive
    -- (Cash homeSide = Debit, so Hat flips it to Credit: the two sides of one
    --  base land in different classes — Deguchi Def 2.13)
    let xCash = (100 .@ (Not :< Cash)) .+ (100 .@ (Hat :< Cash))
                    :: EA.Alg MoneyDecimal (HatBase AccountTitles)
        bySide = EA.decBy (\b -> Just (whichSide b)) xCash
    assertEqual "sentinel: whichSide splits one base across classes (side-sensitive)"
        [Credit, Debit] (L.sort (M.keys bySide))
    assertEqual "sentinel: whichSide decBy does not commute with bar"
        True (M.filter (not . EA.isZero) (M.map bar bySide)
                /= EA.decBy (\b -> Just (whichSide b)) (bar xCash))
    -- sentinel: π_κ (mapBasePart, non-injective) does not commute with bar
    -- (formalization §2.8: coarsen-then-net /= net-then-coarsen)
    let xPi = (100 .@ (Not :< Yen)) .+ (100 .@ (Hat :< Dollar)) :: NNAlg
    assertEqual "sentinel: bar (mapBasePart const) nets across the class"
        0 (norm (bar (EA.mapBasePart (const Amount) xPi :: NNAlg)))
    assertEqual "sentinel: mapBasePart (bar x) keeps both sides (no cross-base netting)"
        200 (norm (EA.mapBasePart (const Amount) (bar xPi) :: NNAlg))

-- ================================================================
-- Bookkeeping closing-adjustment builders (Phase B)
-- ================================================================

type BAlg  = EA.Alg Double      (HatBase AccountTitles)
type BAlgM = EA.Alg MoneyDecimal (HatBase AccountTitles)

mkA :: EB.MkBase (HatBase AccountTitles)
mkA = (:<)

-- balanced-ness: debit-side norm equals credit-side norm (貸借一致)
isBalancedD :: BAlg -> Bool
isBalancedD x = epsEq (norm (EA.decL x)) (norm (EA.decR x))

bookkeepingProperties :: IO ()
bookkeepingProperties = do
    -- (1) balanced property: every builder produces a debit=credit entry
    quickProp "bookkeeping: cogsAdjustmentEntries balanced" $
        forAll genNNDouble $ \beg -> forAll genNNDouble $ \end ->
            isBalancedD (EB.cogsAdjustmentEntries mkA beg end)
    quickProp "bookkeeping: depreciationIndirectEntry balanced" $
        forAll genNNDouble $ \amt -> isBalancedD (EB.depreciationIndirectEntry mkA amt)
    quickProp "bookkeeping: depreciationDirectEntry balanced" $
        forAll genNNDouble $ \amt -> isBalancedD (EB.depreciationDirectEntry mkA amt Fixtures)
    quickProp "bookkeeping: allowanceReplenishmentEntry balanced" $
        forAll genNNDouble $ \est -> forAll genNNDouble $ \cur ->
            isBalancedD (EB.allowanceReplenishmentEntry mkA est cur)
    quickProp "bookkeeping: allowanceResetEntries balanced" $
        forAll genNNDouble $ \est -> forAll genNNDouble $ \cur ->
            isBalancedD (EB.allowanceResetEntries mkA est cur)
    quickProp "bookkeeping: prepaidExpenseEntry balanced" $
        forAll genNNDouble $ \amt -> isBalancedD (EB.prepaidExpenseEntry mkA amt RentExpense)
    quickProp "bookkeeping: unearnedRevenueEntry balanced" $
        forAll genNNDouble $ \amt -> isBalancedD (EB.unearnedRevenueEntry mkA amt RentalIncome)
    quickProp "bookkeeping: accruedRevenueEntry balanced" $
        forAll genNNDouble $ \amt -> isBalancedD (EB.accruedRevenueEntry mkA amt InterestEarned)
    quickProp "bookkeeping: accruedExpenseEntry balanced" $
        forAll genNNDouble $ \amt -> isBalancedD (EB.accruedExpenseEntry mkA amt InterestExpense)
    quickProp "bookkeeping: corporateTaxInterimEntry balanced" $
        forAll genNNDouble $ \amt -> isBalancedD (EB.corporateTaxInterimEntry mkA amt)
    -- consumption / corporate tax settlement: amounts where received >= paid,
    -- total >= interim (the in-scope branch)
    quickProp "bookkeeping: consumptionTaxSettlementEntry balanced (received>=paid)" $
        forAll genNNDouble $ \paid -> forAll genNNDouble $ \extra ->
            isBalancedD (EB.consumptionTaxSettlementEntry mkA paid (paid + extra))
    quickProp "bookkeeping: corporateTaxSettlementEntries balanced (total>=interim)" $
        forAll genNNDouble $ \interim -> forAll genNNDouble $ \extra ->
            isBalancedD (EB.corporateTaxSettlementEntries mkA (interim + extra) interim)
    quickProp "bookkeeping: priorPeriodErrorCorrection balanced" $
        forAll genNNDouble $ \curr -> forAll genNNDouble $ \prior ->
            isBalancedD (EB.priorPeriodErrorCorrection mkA curr prior Depreciation Land)

    -- (2) unit tests: expected bases/amounts on representative lecture figures
    -- COGS (ch.24): beg 100,000 / end 50,000. In isolation this entry's
    -- Purchases net = beg - end = +50,000 (so the Purchases balance becomes COGS),
    -- and MerchandiseInventory net = end - beg = -50,000 (it replaces the opening
    -- balance with the closing one, i.e. 100,000 - 50,000 leaves on the ledger).
    let cogs = EB.cogsAdjustmentEntries mkA 100000 50000 :: BAlg
    assertNear "cogs: Purchases net = beg - end (= cost of goods sold)"
        50000 (signedNet Purchases cogs)
    assertNear "cogs: MerchandiseInventory net = end - beg"
        (-50000) (signedNet MerchandiseInventory cogs)
    -- 差額補充法, estimate>current (ch.16): 1,400 - 1,000 -> provide 400
    let repl1 = EB.allowanceReplenishmentEntry mkA 1400 1000 :: BAlg
    assertNear "allowance(差額補充, shortfall): ProvisionForDoubtfulAccounts = 400"
        400 (norm (EA.projByAccountTitle ProvisionForDoubtfulAccounts repl1))
    assertNear "allowance(差額補充, shortfall): AllowanceForDoubtfulAccounts = 400"
        400 (norm (EA.projByAccountTitle AllowanceForDoubtfulAccounts repl1))
    -- 差額補充法, estimate<current (ch.16): 1,800 - 2,000 -> release 200
    let repl2 = EB.allowanceReplenishmentEntry mkA 1800 2000 :: BAlg
    assertNear "allowance(差額補充, excess): ReversalOfAllowanceForDoubtfulAccounts = 200"
        200 (norm (EA.projByAccountTitle ReversalOfAllowanceForDoubtfulAccounts repl2))
    -- estimate==current -> no entry
    assertEqual "allowance(差額補充, equal): Zero"
        True (EA.isZero (EB.allowanceReplenishmentEntry mkA 1500 1500 :: BAlg))
    -- consumption tax (ch.23): paid 1,000 / received 20,000 -> unpaid 19,000
    let ctax = EB.consumptionTaxSettlementEntry mkA 1000 20000 :: BAlg
    assertNear "consumptionTax: AccruedConsumptionTax = received - paid = 19000"
        19000 (norm (EA.projByAccountTitle AccruedConsumptionTax ctax))
    -- corporate tax (ch.23): total 800,000 / interim 500,000 -> unpaid 300,000
    let crp = EB.corporateTaxSettlementEntries mkA 800000 500000 :: BAlg
    assertNear "corporateTax: AccruedCorporateIncomeTaxes = total - interim = 300000"
        300000 (norm (EA.projByAccountTitle AccruedCorporateIncomeTaxes crp))
    -- prior-period error correction (#15 anchor): patent 55,000/10yr discovered 2028
    -- current 5,500 / prior 2yr 11,000 -> debit=credit=16,500 (Patent credit)
    let ppec = EB.priorPeriodErrorCorrection mkA 5500 11000 AmortizationExpense Patent :: BAlg
    assertNear "priorPeriodErrorCorrection (#15): Patent credit = 16500"
        16500 (norm (EA.projByAccountTitle Patent ppec))
    assertNear "priorPeriodErrorCorrection (#15): balanced (decL == decR)"
        (norm (EA.decL ppec)) (norm (EA.decR ppec))
    -- consumption-tax refund (received<paid) is rejected (out of 3-級 scope)
    rRefund <- try (evaluate (norm (EB.consumptionTaxSettlementEntry mkA 5000 1000 :: BAlg)))
                 :: IO (Either SomeException Double)
    case rRefund of
        Left _  -> putStrLn "[PASS] consumptionTaxSettlementEntry rejects received<paid"
        Right _ -> do putStrLn "[FAIL] consumptionTaxSettlementEntry accepted refund"; exitFailure

    -- (3) reversingEntry: involution + exact cancellation (MoneyDecimal exact)
    quickProp "bookkeeping: reversingEntry is involution (MoneyDecimal)" $
        forAll genBAlgM $ \x -> EB.reversingEntry (EB.reversingEntry x) == x
    quickProp "bookkeeping: bar (x .+ reversingEntry x) == Zero (MoneyDecimal)" $
        forAll genBAlgM $ \x -> bar (x .+ EB.reversingEntry x) == EA.Zero
  where
    -- exact per-account signed net (Not +, Hat -) for Double-based unit checks
    signedNet :: AccountTitles -> BAlg -> Double
    signedNet t = EA.foldEntries step 0
      where step acc v b
              | getAccountTitle b == t = if isHat b then acc - v else acc + v
              | otherwise              = acc

-- small exact MoneyDecimal algebra over AccountTitles bases (closing-adjustment
-- shaped: a few postings on bookkeeping titles), for the reversal properties.
genBAlgM :: Gen BAlgM
genBAlgM = sized $ \n -> do
    k  <- choose (0, min 8 n)
    ps <- vectorOf k ((,) <$> genSmallMoney <*> genBookBase)
    pure (EA.fromList [ v .@ b | (v, b) <- ps ])
  where
    genSmallMoney :: Gen MoneyDecimal
    genSmallMoney = fromInteger <$> choose (1, 9999)
    genBookBase :: Gen (HatBase AccountTitles)
    genBookBase = (:<) <$> elements [Hat, Not]
                       <*> elements [ Purchases, MerchandiseInventory, Depreciation
                                    , AccumulatedDepreciation, PrepaidExpenses
                                    , AccruedExpenses, AccruedConsumptionTax
                                    , Cash, InterestExpense ]

-- ================================================================
-- Closing-document Write functions (Phase D): worksheet,
-- post-closing trial balance, account ledger
-- ================================================================

closingDocsTests :: IO ()
closingDocsTests = do
    -- A small balanced pre-adjustment ledger (ebex1-shaped):
    --   opening capital 2,000,000; a cash sale of 500,000;
    --   wages (cost) 140,000 paid in cash.
    -- Pre-adjustment trial balance balances by construction.
    let pre = (2000000 .@ (Not :< Cash))            -- 現金 (asset, debit)
            .+ (2000000 .@ (Not :< CapitalStock))    -- 資本金 (equity, credit)
            .+ (500000  .@ (Not :< Cash))            -- cash from sale (debit)
            .+ (500000  .@ (Not :< Sales))           -- 売上 (revenue, credit)
            .+ (140000  .@ (Hat :< Cash))            -- cash paid out (credit)
            .+ (140000  .@ (Not :< WageExpenditure)) -- 給料 (cost, debit)
            :: BAlg
    -- one adjustment: accrue 10,000 of unpaid wages (費用の見越し)
    let adj = (10000 .@ (Not :< WageExpenditure))    -- cost debit
            .+ (10000 .@ (Not :< AccruedExpenses))    -- liability credit
            :: BAlg
    let combined = pre .+ adj

    -- (1) Worksheet self-check: the P/L column imbalance must equal the
    --     B/S column imbalance, and both equal the net income.
    --     Each account's *net* balance (diffRL) is routed by division:
    --       P/L: Sales net 500,000 (credit) vs WageExpenditure net 150,000
    --            (debit) => net income 350,000.
    --       B/S: Cash net 2,360,000 (debit) vs CapitalStock 2,000,000 +
    --            AccruedExpenses 10,000 (credit) = 2,010,000 => 350,000.
    --     We compute the column sums the same way worksheetRows does: per
    --     account title, place the *net* balance into the debit or credit
    --     column according to its balance side.
    let titles = L.nub (EA.foldEntries (\acc _ b -> getAccountTitle b : acc) [] combined) :: [AccountTitles]
        netSide t = EA.diffRL (EA.projByAccountTitle t combined) :: (Side, Double)
        colSums divs =
            L.foldl' (\(d,c) t ->
                if classifyAccountDivision t `elem` divs
                  then case netSide t of
                         (Debit,  m) -> (d + m, c)
                         (Credit, m) -> (d, c + m)
                         _           -> (d, c)
                  else (d, c)) (0,0) titles
        (plD, plC) = colSums [Cost, Revenue]
        (bsD, bsC) = colSums [Assets, Liability, Equity]
        plDiff = abs (plD - plC)
        bsDiff = abs (bsD - bsC)
    assertNear "worksheet self-check: P/L diff = 350000" 350000 plDiff
    assertNear "worksheet self-check: B/S diff = 350000" 350000 bsDiff
    assertNear "worksheet self-check: P/L diff == B/S diff (= net income)" plDiff bsDiff

    -- the rendered worksheet's net-income row must carry the same figure on
    -- the P/L debit and B/S credit columns (positions 6 and 9, 1-based).
    let wrows   = EW.worksheetRows pre adj
        netRow' = last (init wrows)   -- penultimate-from-end: the Net row
    assertEqual "worksheet: net row label is Net Income"
        (T.pack "Net Income") (head netRow')
    assertEqual "worksheet: net income on P/L debit column = 350000.0"
        (T.pack "350000.0") (netRow' !! 5)
    assertEqual "worksheet: net income on B/S credit column = 350000.0"
        (T.pack "350000.0") (netRow' !! 8)

    -- (2) Post-closing trial balance must contain only real accounts
    --     (Assets/Liability/Equity) — no Cost/Revenue titles.
    let pcrows = EW.postClosingTrialBalanceRows combined
        titleCells = [ row !! 1 | row <- drop 1 pcrows ]  -- middle column = title
        forbidden  = L.map (T.pack . show) [Sales, WageExpenditure]
    assertEqual "post-closing TB excludes Cost/Revenue titles"
        True (not (any (`elem` forbidden) titleCells))
    assertEqual "post-closing TB includes Cash"
        True (T.pack (show Cash) `elem` titleCells)
    assertEqual "post-closing TB includes AccruedExpenses (liability)"
        True (T.pack (show AccruedExpenses) `elem` titleCells)

    -- (3) Account ledger preserves the seq: the number of posting lines for a
    --     title equals the number of postings on that title (no aggregation).
    --     Cash has 3 postings (2 debit, 1 credit) in `pre`.
    let lrows     = EW.accountLedgerRows [Cash] pre (const dummyDay)
        -- drop the 2 header rows (title + sub-header); the rest are postings.
        bodyLines = drop 2 lrows
        cashPostings = EA.foldEntries (\acc _ b -> if getAccountTitle b == Cash then acc + 1 else acc) (0 :: Int) pre
        -- each body row holds at most one debit + one credit cell; count
        -- non-empty value cells (debit col=1, credit col=3).
        nonEmptyVals = Prelude.length
            [ () | row <- bodyLines, c <- [1,3], not (T.null (row !! c)) ]
    assertEqual "account ledger: Cash posting count preserved (= 3, no aggregation)"
        cashPostings nonEmptyVals
  where
    dummyDay :: Day
    dummyDay = fromGregorian 2024 4 1

-- ================================================================
-- Simulate.Lite tests (Phase 2, feat/simulate-lite)
-- ================================================================

-- A concrete Note type for the Lite models: (event tag, term index).
type LNote   = (String, Int)
type LBaseD  = HatBase AccountTitles
type LedgerD = Journal LNote MoneyDouble LBaseD     -- IEEE-754 path (DET-1, BSP, equiv)
type LBaseM  = HatBase AccountTitles
type LedgerM = Journal LNote MoneyDecimal LBaseM    -- exact path (DET-2)

------------------------------------------------------------------
-- Lite test 1: boilerplate acceptance example (3 fields, 2 stages).
-- The body of this function (the World record, the two stages, the spec and
-- the run) is the "~20 line" boilerplate the design targets.
------------------------------------------------------------------

-- A product-only HKD world: a ledger, a scalar price, a scalar tax rate.
data MiniW f = MiniW
  { mwLedger :: HK f LedgerD
  , mwPrice  :: HK f MoneyDouble
  , mwTax    :: HK f Double
  } deriving Generic

-- stage A: each agent buys 1 unit at the snapshot price (a pure message).
buyStage :: Stage MiniW Int LNote MoneyDouble LBaseD
buyStage = stageFor "buy" [1 .. 5 :: Int] $ \w t _g i ->
    let amt = mwPrice w * fromIntegral i
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("buy", t)

-- stage B: a single bookkeeping step paying tax on the snapshot price.
taxStage :: Stage MiniW Int LNote MoneyDouble LBaseD
taxStage = stage "tax" $ \w t ->
    let amt = mwPrice w * realToFrac (mwTax w)
    in ((amt .@ Not :< Sales) .+ (amt .@ Hat :< Cash)) .| ("tax", t)

miniSpec :: SimSpec MiniW Int LNote MoneyDouble LBaseD
miniSpec = mkSimSpec (1, 3) 42 mwLedger [buyStage, taxStage]

testLiteBoilerplate :: IO ()
testLiteBoilerplate = do
    let w0 = MiniW { mwLedger = carry mempty
                   , mwPrice  = carry 10
                   , mwTax    = carry 0.1 }
        n  = runLite miniSpec w0 (realToFrac . norm . mwLedger)
    -- 3 terms * (sum_{i=1..5} 10*i*2  +  10*0.1*2) = 3 * (300 + 2) = 906
    assertNear "Lite: boilerplate mini-model runs (norm)" 906.0 n

------------------------------------------------------------------
-- Lite test 2 (DET-2): MoneyDecimal Sequential vs ParChunk exact match.
------------------------------------------------------------------

data DecW f = DecW
  { dwLedger :: HK f LedgerM
  } deriving Generic

decStage :: Stage DecW Int LNote MoneyDecimal LBaseM
decStage = stageFor "post" [1 .. 50 :: Int] $ \_w t g i ->
    let (k, _) = randomR (1, 9 :: Int) g
        amt    = fromIntegral (i + k) :: MoneyDecimal
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("post", t)

decSpec :: Par -> SimSpec DecW Int LNote MoneyDecimal LBaseM
decSpec par = (mkSimSpec (1, 4) 7 dwLedger [decStage]) { Lite.specParallel = par }

testLiteDet2 :: IO ()
testLiteDet2 = do
    let w0 = DecW { dwLedger = carry mempty }
        runP par = runLite (decSpec par) w0 (toMap . dwLedger)
        seqMap = runP Sequential
        parMap = runP (ParChunk 8)
    assertEqual "Lite DET-2: Sequential and ParChunk produce identical ledgers (exact)"
        seqMap parMap

------------------------------------------------------------------
-- Lite test 3 (DET-1): MoneyDouble reproducibility across two runs.
------------------------------------------------------------------

testLiteDet1 :: IO ()
testLiteDet1 = do
    let w0 = MiniW { mwLedger = carry mempty
                   , mwPrice  = carry 10
                   , mwTax    = carry 0.1 }
        n1 = runLite miniSpec w0 (realToFrac . norm . mwLedger)
        n2 = runLite miniSpec w0 (realToFrac . norm . mwLedger)
    assertNear "Lite DET-1: same spec run twice gives same norm" n1 n2

------------------------------------------------------------------
-- Lite test 4 (BSP intra-stage invisibility sentinel).
-- Every agent in a stage reads the SAME snapshot. We encode the snapshot
-- ledger's norm into each agent's message; if a later agent could see an
-- earlier agent's write within the same stage, the encoded norms would differ
-- from the all-zero baseline (the ledger starts empty for term 1 stage 0).
------------------------------------------------------------------

data BspW f = BspW
  { bwLedger :: HK f LedgerD
  } deriving Generic

-- each agent posts (1 + norm-of-snapshot-ledger). On term 1, stage 0, the
-- snapshot ledger is empty for every agent, so each posts exactly 1.0.
bspStage :: Stage BspW Int LNote MoneyDouble LBaseD
bspStage = stageFor "bsp" [1 .. 10 :: Int] $ \w t _g _i ->
    let seenNorm = norm (bwLedger w)          -- must be 0 for ALL agents (BSP)
        amt = 1 + realToFrac seenNorm :: MoneyDouble
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("bsp", t)

bspSpec :: SimSpec BspW Int LNote MoneyDouble LBaseD
bspSpec = mkSimSpec (1, 1) 0 bwLedger [bspStage]

testLiteBspInvisibility :: IO ()
testLiteBspInvisibility = do
    let w0 = BspW { bwLedger = carry mempty }
        n  = runLite bspSpec w0 (realToFrac . norm . bwLedger)
    -- 10 agents each post Not:<Purchases 1.0 + Hat:<Cash 1.0 = norm 20.
    -- If intra-stage writes were visible, later agents would post > 1.0 and the
    -- norm would exceed 20.
    assertNear "Lite BSP: intra-stage invisibility (all agents see empty ledger)"
        20.0 n

------------------------------------------------------------------
-- Lite test 5: gate toy-model equivalence (3 terms, agents [1..10], norm 3300).
-- Rebuilds the gate-report.md prototype with the Lite API; same norm.
------------------------------------------------------------------

data GateW f = GateW
  { gwLedger :: HK f LedgerD
  , gwPrice  :: HK f MoneyDouble
  } deriving Generic

gateStage :: Stage GateW Int LNote MoneyDouble LBaseD
gateStage = stageFor "buy" [1 .. 10 :: Int] $ \w t _g i ->
    let amt = gwPrice w * fromIntegral i
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("buy", t)

gateSpec :: SimSpec GateW Int LNote MoneyDouble LBaseD
gateSpec = mkSimSpec (1, 3) 1 gwLedger [gateStage]

testLiteGateEquivalence :: IO ()
testLiteGateEquivalence = do
    let w0 = GateW { gwLedger = carry mempty, gwPrice = carry 10 }
        n  = runLite gateSpec w0 (realToFrac . norm . gwLedger)
    -- norm = 3 terms * sum_{i=1..10} (10*i*2) = 3 * 2 * 10 * 55 = 3300
    assertNear "Lite: gate toy-model equivalence (norm 3300)" 3300.0 n

------------------------------------------------------------------
-- Lite test 6: term-boundary Field rules (Carry / ResetEach / UpdateEach).
-- One agent posts (current price) each term; the three runs differ only in the
-- price field's boundary rule, exercising each Field constructor.
------------------------------------------------------------------

data RuleW f = RuleW
  { rwLedger :: HK f LedgerD
  , rwPrice  :: HK f MoneyDouble
  } deriving Generic

ruleStage :: Stage RuleW Int LNote MoneyDouble LBaseD
ruleStage = stage "post" $ \w t ->
    let amt = rwPrice w
    in ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("post", t)

ruleSpec :: SimSpec RuleW Int LNote MoneyDouble LBaseD
ruleSpec = mkSimSpec (1, 3) 0 rwLedger [ruleStage]

runRule :: Field MoneyDouble -> Double
runRule priceField =
    let w0 = RuleW { rwLedger = carry mempty, rwPrice = priceField }
    in realToFrac (runLite ruleSpec w0 (norm . rwLedger))
       -- norm counts both Not:<Purchases and Hat:<Cash, hence 2 * price each term

testLiteFieldRules :: IO ()
testLiteFieldRules = do
    -- Carry 10: price stays 10 every term -> 3 * 2 * 10 = 60
    assertNear "Lite Field: Carry keeps the value" 60.0 (runRule (carry 10))
    -- ResetEach 5: price reset to 5 at each boundary, but stage reads BEFORE
    -- the term-1 boundary commit at the same value -> 3 * 2 * 5 = 30
    assertNear "Lite Field: ResetEach restores each term" 30.0 (runRule (resetEach 5))
    -- UpdateEach 10 (*2): term1 price 10, boundary doubles -> term2 20, term3 40.
    -- norm = 2 * (10 + 20 + 40) = 140
    assertNear "Lite Field: UpdateEach applies the step each boundary"
        140.0 (runRule (updateEach 10 (* 2)))

-- regression: the boundary rule must fire once per TERM, not per stage.
-- (Parser pitfall: a trailing backtick operator after an inner lambda's
-- do-block is swallowed into the lambda body, turning the term-boundary
-- commit into a per-stage commit. Single-stage tests cannot see this.)
-- 2 identical stages x UpdateEach 10 (*2): both stages must read the SAME
-- price within a term -> norm = 2 entries * 2 stages * (10+20+40) = 280.
-- The per-stage-commit bug yields 2 * (10+20 + 40+80 + 160+320) = 1260.
testLiteBoundaryOncePerTerm :: IO ()
testLiteBoundaryOncePerTerm =
    assertNear "Lite: term boundary fires once per term (2 stages)" 280.0
        (let spec2 = mkSimSpec (1, 3) 0 rwLedger [ruleStage, ruleStage]
             w0 = RuleW { rwLedger = carry mempty, rwPrice = updateEach 10 (* 2) }
         in realToFrac (runLite spec2 w0 (norm . rwLedger)))

-- ================================================================
-- Simulate.Policy tests (Phase 4, feat/ledger-policy)
--
-- LedgerPolicy = declarative retention / spill / compaction, applied at the
-- term boundary by runLiteWithPolicy. The exact MoneyDecimal value type lets us
-- assert lossless round-trips and norm/compaction invariants by strict equality.
-- ================================================================

-- A one-field world whose stage posts a few distinct bases per term, so that a
-- closed term has redundant per-base sequences (exercising CompressClosedTerms)
-- and a multi-term history (exercising RetainRecent + spill).
data PolW f = PolW
  { pwLedger :: HK f LedgerM
  } deriving Generic

-- A single-field world for the classic-bridge test: a constructor @a@ of kind
-- @Type -> Type@ whose @a RealWorld@ is an @STRef RealWorld LedgerM@ (so it fits
-- the @SpillOptions t a payload@ shape, where @a@ is applied to the state token).
newtype LedgerRef s = LedgerRef (STRef s LedgerM)

-- Each agent posts twice to the SAME base within the term, so the term's per-base
-- posting sequence has length 2 before compress and length 1 after.
polStage :: Stage PolW Int LNote MoneyDecimal LBaseM
polStage = stageFor "post" [1 .. 4 :: Int] $ \_w t _g i ->
    let amt = fromIntegral i :: MoneyDecimal
        one = 1             :: MoneyDecimal
        m1  = ((amt .@ Not :< Purchases) .+ (amt .@ Hat :< Cash)) .| ("post", t)
        m2  = ((one .@ Not :< Purchases) .+ (one .@ Hat :< Cash)) .| ("post", t)
    in m1 .+ m2 :: Journal LNote MoneyDecimal LBaseM

polSpec :: SimSpec PolW Int LNote MoneyDecimal LBaseM
polSpec = mkSimSpec (1, 5) 0 pwLedger [polStage]

polW0 :: PolW InitT
polW0 = PolW { pwLedger = carry mempty }

-- run a temp spill file, returning (result, path); caller removes the file.
withTempSpill :: String -> (FilePath -> IO a) -> IO a
withTempSpill tag act = do
    let path = "/tmp/exchangealgebra_policy_" ++ tag ++ ".bin"
    -- ensure no stale file from a previous run (append-mode would accumulate)
    _ <- try (removeFile path) :: IO (Either SomeException ())
    r <- act path
    _ <- try (removeFile path) :: IO (Either SomeException ())
    pure r

-- Test 1 (flagship): defaultLedgerPolicy is observationally equal to runLite.
testPolicyEquivalence :: IO ()
testPolicyEquivalence = do
    let pureLedger = runLite polSpec polW0 (toMap . pwLedger)
    polLedger <- runLiteWithPolicy Policy.defaultLedgerPolicy polSpec polW0 (toMap . pwLedger)
    assertEqual "Policy: runLiteWithPolicy defaultLedgerPolicy == runLite (exact)"
        pureLedger polLedger

-- Test 2 (flagship): RetainRecent w + spillTo gives an in-memory window AND a
-- lossless restore that equals the FullAudit ledger.
testPolicyWindowRoundTrip :: IO ()
testPolicyWindowRoundTrip = withTempSpill "window" $ \path -> do
    let full = runLite polSpec polW0 (toMap . pwLedger)   -- FullAudit reference
        pol  = Policy.defaultLedgerPolicy
                 { Policy.retain  = Policy.RetainRecent 2
                 , Policy.spillTo = Just path }
    -- ONE policy run (append-mode spill: a second run would double the file),
    -- projecting the live journal; we derive both checks from it.
    residentJournal <- runLiteWithPolicy pol polSpec polW0 pwLedger
    -- (a) in-memory ledger after the run contains ONLY the most recent 2 terms.
    let residentMap   = toMap residentJournal
        residentTerms = L.sort (L.nub [ t | (_, t) <- HM.keys residentMap ])
    assertEqual "Policy: RetainRecent 2 leaves only the most recent 2 terms resident"
        [4, 5] residentTerms
    -- (b) restoreLedger (spill file + resident remainder) == FullAudit ledger.
    restored <- Policy.restoreLedger path residentJournal :: IO LedgerM
    assertEqual "Policy: restoreLedger (spill + remainder) == FullAudit ledger (lossless, exact)"
        full (toMap restored)

-- Test 3: CompressClosedTerms — norm/balance invariant, closed-term seq length 1,
-- in-progress term keeps full redundancy.
testPolicyCompressClosed :: IO ()
testPolicyCompressClosed = do
    let full = runLite polSpec polW0 (toMap . pwLedger)
    compactedJ <- runLiteWithPolicy
                    (Policy.defaultLedgerPolicy { Policy.compaction = Policy.CompressClosedTerms })
                    polSpec polW0 pwLedger
    let fullJ = runLite polSpec polW0 pwLedger
    -- (a) norm is invariant under compaction.
    assertEqual "Policy: CompressClosedTerms preserves norm (exact)"
        (norm fullJ) (norm compactedJ)
    -- (b) balance result unchanged (still balanced overall).
    assertEqual "Policy: CompressClosedTerms preserves balance"
        (EA.balance fullJ) (EA.balance compactedJ)
    -- (c) each CLOSED term (1..4) has at most one posting per base/side: its Alg
    --     compresses to itself (idempotent), so compress . entry == entry.
    let compactedMap = toMap compactedJ
        closedOk = all
          (\((_, t), alg) -> t == (5 :: Int) || EA.compress alg == alg)
          (HM.toList compactedMap)
    assertEqual "Policy: closed terms are already compressed (compress is a no-op on them)"
        True closedOk
    -- (d) the in-progress term (5) keeps its redundancy: in the FULL ledger term
    --     5's entry has a length-2 sequence, and the compacted ledger keeps the
    --     SAME term-5 entry (untouched), i.e. it differs from its own compress.
    let term5Full = HM.lookup ("post", 5) (toMap fullJ)
        term5Comp = HM.lookup ("post", 5) compactedMap
    assertEqual "Policy: in-progress term is untouched by CompressClosedTerms"
        term5Full term5Comp
    case term5Comp of
      Just alg -> assertEqual "Policy: in-progress term retains its redundant sequence"
                    False (EA.compress alg == alg)
      Nothing  -> assertEqual "Policy: in-progress term present" True False

-- Test 4: deletion-only (spillTo Nothing + RetainRecent) narrows the ledger to
-- the window and reduces its norm by exactly the discarded terms' norm.
testPolicyDeleteOnly :: IO ()
testPolicyDeleteOnly = do
    let pol = Policy.defaultLedgerPolicy { Policy.retain = Policy.RetainRecent 2 }
    residentJournal <- runLiteWithPolicy pol polSpec polW0 pwLedger
    let residentMap = toMap residentJournal
        residentTerms = L.sort (L.nub [ t | (_, t) <- HM.keys residentMap ])
        -- the FullAudit ledger restricted to the same window must match exactly
        -- (deletion is just a filter; the kept terms are untouched).
        full = runLite polSpec polW0 pwLedger
        windowOfFull = EJ.filterWithNote (\(_, t) _ -> t >= 4) full
    assertEqual "Policy: delete-only leaves only the window terms" [4, 5] residentTerms
    assertEqual "Policy: delete-only window equals FullAudit restricted to the window (exact)"
        (toMap windowOfFull) residentMap
    -- norm strictly drops (terms 1..3 were discarded with no spill).
    assertEqual "Policy: discarding older terms strictly reduces norm"
        True (norm residentJournal < norm full)

-- Test 5: DET — policy runs are reproducible and Sequential == ParChunk (exact).
testPolicyDeterminism :: IO ()
testPolicyDeterminism = withTempSpill "det" $ \_ -> do
    let pol = Policy.defaultLedgerPolicy { Policy.retain = Policy.RetainRecent 3 }
        specPar p = polSpec { Lite.specParallel = p }
    r1 <- runLiteWithPolicy pol (specPar Sequential) polW0 (toMap . pwLedger)
    r2 <- runLiteWithPolicy pol (specPar Sequential) polW0 (toMap . pwLedger)
    rP <- runLiteWithPolicy pol (specPar (ParChunk 2)) polW0 (toMap . pwLedger)
    assertEqual "Policy DET-1: same policy run twice is identical" r1 r2
    assertEqual "Policy DET-2: Sequential == ParChunk under policy (exact)" r1 rP

-- Test 6 (classic bridge): policySpillOptions drives the classic engine and the
-- result restores losslessly, mirroring the existing binary-spill restore test.
-- We exercise the derived chunk extraction + eviction directly (no full
-- StateSpace needed) by checking the option fields it builds.
testPolicyClassicBridge :: IO ()
testPolicyClassicBridge = withTempSpill "bridge" $ \path -> do
    -- Build a ledger spanning terms 1..3, spill terms 1..2 via the policy-derived
    -- chunk extractor, keep term 3 as the remainder, then restore == whole ledger.
    let pol = Policy.defaultLedgerPolicy
                { Policy.retain = Policy.RetainRecent 1, Policy.spillTo = Just path }
        whole :: LedgerM
        whole = EJ.fromList
            [ (1 .@ Not :< Purchases) .| ("post", 1)
            , (2 .@ Not :< Purchases) .| ("post", 2)
            , (3 .@ Not :< Purchases) .| ("post", 3) ]
        -- the option built by the bridge; we use its spillExtractChunk to carve
        -- terms 1..2 and write them, exactly as runSimulationWithSpill would.
        opts = Policy.policySpillOptions pol 2
                 (\(LedgerRef r) -> readSTRef r)
                 (\f (LedgerRef r) -> modifySTRef' r f)
                 :: ES.SpillOptions Int LedgerRef LedgerM
    -- emulate a single spill of the [1,2] chunk + eviction of term <= 2.
    ref <- LedgerRef <$> stToIO (newSTRef whole)
    chunk <- case ES.spillExtractChunk opts of
        Just extract -> stToIO (extract (1, 2) ref)
        Nothing      -> error "policySpillOptions must set spillExtractChunk"
    withFile path WriteMode $ \h -> ES.spillWriteChunk opts h (1, 2) chunk
    stToIO (ES.spillDeleteRange opts (1, 2) ref)
    let LedgerRef r0 = ref
    remainder <- stToIO (readSTRef r0)
    -- remainder is now only term 3; restore merges spill + remainder == whole.
    restored <- Policy.restoreLedger path remainder :: IO LedgerM
    assertEqual "Policy bridge: policySpillOptions chunk keeps spilled-range terms"
        (toMap (EJ.filterWithNote (\(_, t) _ -> t >= 1 && t <= 2) whole)) (toMap chunk)
    assertEqual "Policy bridge: after eviction the remainder is only the kept window"
        (toMap (EJ.filterWithNote (\(_, t) _ -> t > 2) whole)) (toMap remainder)
    assertEqual "Policy bridge: restore (spill + remainder) == whole ledger (lossless)"
        (toMap whole) (toMap restored)

-- Test 7 (HasTermAxis): termOf returns the LAST Note component for pair/triple.
testPolicyHasTermAxis :: IO ()
testPolicyHasTermAxis = do
    assertEqual "Policy HasTermAxis: pair termOf = snd" (7 :: Int) (Policy.termOf ("e", 7 :: Int))
    assertEqual "Policy HasTermAxis: triple termOf = 3rd" (9 :: Int)
        (Policy.termOf ("e1", "e2", 9 :: Int))

-- ================================================================
-- Simulate.Network tests (Phase 3, feat/trade-network)
--
-- Property + unit tests for the TradeNetwork / InputCoefficients separation,
-- the deterministic generators, the smart-constructor invariants, and the
-- edge-summation sigmaEdges. All read-outs are Ord-ascending (no hash order).
-- ================================================================

type NetJD = Journal (Int, Int) MoneyDecimal (HatBase CountUnit)

-- a tiny per-edge journal builder used by the sigmaEdges equivalence test
edgeJ :: Int -> Int -> NetJD
edgeJ i j = ((fromIntegral (i + 2 * j) :: MoneyDecimal) .@ Not :< Amount) .| (i, j)

-- Test 1: completeNetwork makes sigmaEdges coincide with the all-pairs sum.
-- "The notation is unchanged; only the set Σ runs over changes."
testNetCompleteEquiv :: IO ()
testNetCompleteEquiv = do
    let ks = [1 .. 6 :: Int]
        viaEdges = sigmaEdges (completeNetwork ks) edgeJ          :: NetJD
        viaPairs = EJ.sigma2When ks ks (/=) edgeJ                 :: NetJD
    assertEqual "Network: sigmaEdges complete == all-pairs sigma2When (exact)"
        (toMap viaPairs) (toMap viaEdges)

-- Test 2: determinism (DET-1). Same StdGen -> identical edges for every
-- generator, checked by running each twice and comparing.
testNetDeterminism :: IO ()
testNetDeterminism = do
    let ks = [1 .. 30 :: Int]
        g  = mkStdGen 42
        twice f = assertEqual ("Network DET-1: " ++ fst f) (edges (snd f g)) (edges (snd f g))
    twice ("kRegular",   \s -> kRegular   s ks 4)
    twice ("erdosRenyi", \s -> erdosRenyi s ks 0.3)
    twice ("scaleFree",  \s -> scaleFree  s ks 3)
    twice ("sectorBlock",\s -> sectorBlock s [(k, k `mod` 3) | k <- ks] (\(a,b) -> if a==b then 0.5 else 0.1))

-- Test 3: smart constructors reject the four illegal cases.
testNetSmartConstructor :: IO ()
testNetSmartConstructor = do
    assertEqual "Network: self-loop rejected"
        (Left SelfLoop) (tradeNetwork [1,2] [(1,1)] :: Either NetworkError (TradeNetwork Int))
    assertEqual "Network: duplicate edge rejected"
        (Left DuplicateEdge) (tradeNetwork [1,2] [(1,2),(1,2)] :: Either NetworkError (TradeNetwork Int))
    let Right g = tradeNetwork [1,2,3] [(1,3)] :: Either NetworkError (TradeNetwork Int)
    assertEqual "Network: coefficient outside network rejected"
        (Left CoefOutsideNetwork)
        (inputCoefficients g [(2,3,0.5)] :: Either NetworkError (InputCoefficients Int Double))
    assertEqual "Network: negative coefficient rejected"
        (Left NegativeCoefficient)
        (inputCoefficients g [(1,3,-0.5)] :: Either NetworkError (InputCoefficients Int Double))
    assertEqual "Network: duplicate coefficient rejected"
        (Left DuplicateCoefficient)
        (inputCoefficients g [(1,3,0.2),(1,3,0.3)] :: Either NetworkError (InputCoefficients Int Double))

-- Test 4: Hawkins-Simon — every buyer's column sum is strictly below 1.
testNetHawkinsSimon :: IO ()
testNetHawkinsSimon = do
    let g  = completeNetwork [1 .. 12 :: Int]
        a  = randomCoefficients (mkStdGen 11) defaultCoefOptions g :: InputCoefficients Int Double
        ok = all (\j -> sum (Prelude.map snd (inputsOf a j)) < 1.0) (nodes g)
    assertEqual "Network: randomCoefficients (hawkinsSimon) all column sums < 1" True ok

-- Test 5: generator structure.
testNetGeneratorStructure :: IO ()
testNetGeneratorStructure = do
    let ks = [1 .. 8 :: Int]
        kr = kRegular (mkStdGen 3) ks 3 :: TradeNetwork Int
    assertEqual "Network: kRegular in-degree = min k (N-1)"
        (replicate (length ks) 3)
        (Prelude.map (length . suppliersOf kr) (nodes kr))
    -- erdosRenyi p=1 == complete, p=0 == empty
    assertEqual "Network: erdosRenyi p=1 == completeNetwork edges"
        (edges (completeNetwork ks))
        (edges (erdosRenyi (mkStdGen 0) ks 1.0 :: TradeNetwork Int))
    assertEqual "Network: erdosRenyi p=0 has no edges"
        0 (edgeCount (erdosRenyi (mkStdGen 0) ks 0.0 :: TradeNetwork Int))
    -- scaleFree edge count is deterministic: C(m+1,2) + (N-m-1)*m
    let n = length ks; m = 2
        expected = (m * (m + 1) `div` 2) + (n - m - 1) * m
    assertEqual "Network: scaleFree edge count matches preferential-attachment formula"
        expected (edgeCount (scaleFree (mkStdGen 9) ks m :: TradeNetwork Int))

-- Test 6: out/in adjacency consistency on an arbitrary generated network.
-- (i,j) in edges  <=>  i in suppliersOf j  <=>  j in buyersOf i
testNetAdjacencyConsistency :: IO ()
testNetAdjacencyConsistency = do
    let ks = [1 .. 25 :: Int]
        g  = erdosRenyi (mkStdGen 77) ks 0.25 :: TradeNetwork Int
        es = edges g
        fwd = all (\(i,j) -> i `elem` suppliersOf g j && j `elem` buyersOf g i) es
        -- and the reverse: every (i,j) reconstructed from suppliersOf equals edges
        viaSuppliers = L.sort [ (i, j) | j <- nodes g, i <- suppliersOf g j ]
        viaBuyers    = L.sort [ (i, j) | i <- nodes g, j <- buyersOf g i ]
    assertEqual "Network: edges <=> suppliersOf (forward)" True fwd
    assertEqual "Network: edges == reconstruction from suppliersOf" (L.sort es) viaSuppliers
    assertEqual "Network: edges == reconstruction from buyersOf" (L.sort es) viaBuyers

-- Test 7: CSV round-trip — parse . render == id (render is a test helper).
renderEdgeCsv :: [(T.Text, T.Text)] -> T.Text
renderEdgeCsv rows = T.unlines (T.pack "from,to" : [ a <> T.pack "," <> b | (a, b) <- rows ])

renderCoefCsv :: [(T.Text, T.Text, Double)] -> T.Text
renderCoefCsv rows =
    T.unlines (T.pack "from,to,coef" :
        [ a <> T.pack "," <> b <> T.pack "," <> T.pack (show c) | (a, b, c) <- rows ])

testNetCsvRoundTrip :: IO ()
testNetCsvRoundTrip = do
    let eRows = [(T.pack "a", T.pack "b"), (T.pack "b", T.pack "c"), (T.pack "a", T.pack "c")]
    assertEqual "Network: edge CSV parse . render == id"
        (Right eRows) (parseEdgeCsv (renderEdgeCsv eRows))
    let cRows = [(T.pack "a", T.pack "b", 0.25), (T.pack "b", T.pack "c", 0.5)]
    assertEqual "Network: coef CSV parse . render == id"
        (Right cRows) (parseCoefCsv (renderCoefCsv cRows))
    -- ingestion helpers agree with the network/coefficient invariants
    let Right (g, a) = coefficientsFromTable [(1,3,0.2),(2,3,0.5)]
                         :: Either NetworkError (TradeNetwork Int, InputCoefficients Int Double)
    assertEqual "Network: coefficientsFromTable edges" [(1,3),(2,3)] (edges g)
    assertEqual "Network: coefficientsFromTable inputsOf" [(1,0.2),(2,0.5)] (inputsOf a 3)
    -- fromCoefficientMatrix drops zero cells from the support
    let m i j = if i < j then fromIntegral (i + j) else 0 :: Double
        (gm, am) = fromCoefficientMatrix [1,2,3 :: Int] m
    assertEqual "Network: fromCoefficientMatrix support drops zeros"
        [(1,2),(1,3),(2,3)] (edges gm)
    assertEqual "Network: fromCoefficientMatrix coefficient" (Just 4.0) (coefficient am 1 3)
    -- networkFromTable derives nodes from rows
    let Right gt = networkFromTable [(1,2),(2,3)] :: Either NetworkError (TradeNetwork Int)
    assertEqual "Network: networkFromTable derives node set" [1,2,3] (nodes gt)

-- ================================================================
-- MarketModel equivalence tests (Phase 5, feat/market-scale-experiments)
--
-- The examples/market/MarketModel.hs core cannot be imported here (it declares
-- an orphan `instance StateTime Int` that would clash with the SICE harness's
-- `instance StateTime SimTerm`), so the trade simple/tuned stages and a small
-- BSP world are re-stated minimally (per the Phase 5 plan §2 commit 3 note).
-- We check the two properties the plan puts in CI:
--   (a) tradeStageSimple ≡ tradeStageTuned, EXACTLY, under MoneyDecimal;
--   (b) Sequential ≡ ParChunk (DET-2) for the whole 3-stage model, exactly.
-- (Perf ratios are out of CI; they live in run-market-experiments.sh.)
-- ================================================================

-- 4-axis base (AccountTitles, owner, counterparty, CountUnit), mirroring
-- MarketModel.MBase. It is exactly the SICE harness's SimHatBase2, so we reuse
-- that type (and its ExBaseClass / Element Int / BaseClass Int instances)
-- instead of re-declaring them.
type MktFirm  = SimCompany           -- = Int

-- ADT event tag mirroring MarketModel.MTag (typo'd tags become compile errors,
-- not silently-empty projections). 'MktPlank' is the explicit blank tag.
data MktTag = MktPlank | MktTrade | MktProduction | MktReport | MktClosing | MktCarryover
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance Hashable MktTag
-- needed so the spill / runLiteWithPolicy window-transparency test can serialise
-- a @Journal MktNote v b@ (derived structurally from Generic).
instance Binary.Binary MktTag
instance Note MktTag where
    plank = MktPlank

type MktNote  = (MktTag, Int)
type MktBase  = SimHatBase2          -- = HatBase (AccountTitles, Int, Int, CountUnit)
type MktLedgM = Journal MktNote MoneyDecimal MktBase

data MktW v f = MktW
  { mkLedger :: HK f (Journal MktNote v MktBase)
  , mkNet    :: HK f (TradeNetwork MktFirm)
  , mkCoef   :: HK f (InputCoefficients MktFirm v)
  } deriving Generic

-- own-product classifier shared by the mirror.
mktOwnerOfProduct :: BasePart MktBase -> Maybe MktFirm
mktOwnerOfProduct bp = case bp of
    (Products, o, c, _) | o == c -> Just o
    _                            -> Nothing

-- opening stock read from the (MktCarryover, t) note (indexed per-note),
-- mirroring MarketModel.openingMap (carryover-based O(term) inventory).
mktOpening :: (HatVal v, Real v)
           => Int -> Journal MktNote v MktBase -> M.Map MktFirm v
mktOpening t ledger =
    EA.balanceMapBy mktOwnerOfProduct
        (EJ.toAlg (EJ.projWithNote [(MktCarryover, t)] ledger))

-- single-firm opening read (indexed per-note + per-base), mirroring
-- MarketModel.openingOf: balanceBy over firm j's own-product base only.
mktOpeningOf :: (HatVal v, Real v)
             => Int -> MktFirm -> Journal MktNote v MktBase -> v
mktOpeningOf t j ledger =
    EA.balanceBy [Not :< (Products, j, j, Amount)]
                 [Hat :< (Products, j, j, Amount)]
                 (EJ.toAlg (EJ.projWithNote [(MktCarryover, t)] ledger))

-- single-firm inventory-connected demand, mirroring MarketModel.demandOf.
mktDemandOf :: (HatVal v, Real v)
            => Double -> Int -> MktFirm -> Journal MktNote v MktBase -> Double
mktDemandOf target t j ledger =
    max 0 (target - realToFrac (mktOpeningOf t j ledger))

mktPurchase :: (HatVal v) => v -> MktFirm -> MktFirm -> EA.Alg v MktBase
mktPurchase amt i j =
       amt .@ Not :< (Products,  j, j, Amount)
  .+   amt .@ Hat :< (Cash,      j, j, Yen)
  .+   amt .@ Not :< (Purchases, j, j, Yen)
  .+   amt .@ Not :< (Cash,      i, i, Yen)
  .+   amt .@ Not :< (Sales,     i, i, Yen)
  .+   amt .@ Hat :< (Products,  i, i, Amount)

mktOrderAmt :: (HatVal v, Real v)
            => InputCoefficients MktFirm v -> Double -> MktFirm -> MktFirm -> Double
mktOrderAmt coef d i j =
    realToFrac (maybe 0 id (coefficient coef i j)) * d

-- per-firm trade stage (stageFor over the firm list), mirroring
-- MarketModel.tradeStageSimple: buyer j folds its in-edges (suppliersOf).
mktTradeSimple :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktTradeSimple fs target = stageOf MktTrade fs $ \w t _g j ->
    -- single-note stage: emit the bare Alg; the runner attaches (MktTrade, t).
    let net = mkNet w; coef = mkCoef w
        d   = mktDemandOf target t j (mkLedger w)
        sup = suppliersOf net j
        one i = let amt = realToFrac (mktOrderAmt coef d i j)
                in if amt <= 0 then mempty else mktPurchase amt i j
    in EA.sigma sup one

mktTradeTuned :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktTradeTuned fs target = stageOf MktTrade fs $ \w t _g j ->
    -- single-note stage: see 'mktTradeSimple'.
    let net = mkNet w; coef = mkCoef w
        d   = mktDemandOf target t j (mkLedger w)
        sup = suppliersOf net j
        accum = L.foldl' step M.empty sup
        step acc i =
            let amt = realToFrac (mktOrderAmt coef d i j)
            in if amt <= 0 then acc
               else L.foldl' (\m (b, v) -> M.insertWith (+) b v m) acc
                      [ (Not :< (Products,  j, j, Amount), amt)
                      , (Hat :< (Cash,      j, j, Yen),    amt)
                      , (Not :< (Purchases, j, j, Yen),    amt)
                      , (Not :< (Cash,      i, i, Yen),    amt)
                      , (Not :< (Sales,     i, i, Yen),    amt)
                      , (Hat :< (Products,  i, i, Amount), amt) ]
    in EA.sigmaFromMap accum (\b v -> v .@ b)

mktProduction :: (HatVal v, Real v) => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktProduction fs target = stageOf MktProduction fs $ \w t _g j ->
    -- single-note stage: emit the bare Alg; the runner attaches (MktProduction, t).
    let amt = realToFrac (mktDemandOf target t j (mkLedger w))
    in if amt <= 0 then mempty
       else (amt .@ Hat :< (Products,  j, j, Amount))
         .+ (amt .@ Not :< (SalesCost, j, j, Yen))

mktReport :: (HatVal v) => Stage (MktW v) Int MktNote v MktBase
mktReport = stageOf MktReport [()] $ \w t _g () ->
    -- single-note aggregate stage: emit the bare Alg; runner attaches (MktReport, t).
    let flow = EJ.toAlg (EJ.projWithNote [(MktTrade, t), (MktProduction, t)] (mkLedger w))
        shortageK b = case b of
            Hat :< (Products, o, c, _) | o == c -> Just o
            _                                   -> Nothing
    in EA.postFromNetBy shortageK (\j v -> v .@ Not :< (Products, j, j, Amount)) flow

-- carryover stage (mirror): net this term's own-product stock and roll the
-- positive surplus into (MktCarryover, t+1). Mirrors MarketModel.carryoverStage.
mktCarryover :: (HatVal v, Real v) => Stage (MktW v) Int MktNote v MktBase
mktCarryover = stage "closing" $ \w t ->
    let termAlg = EJ.toAlg (EJ.filterByAxis 1 (NoteAxisKey (t :: Int)) (mkLedger w))
        netMap  = EA.balanceMapBy mktOwnerOfProduct termAlg
        perFirm (j, v) =
            if v <= 0 then mempty
            else ((v .@ Hat :< (Products, j, j, Amount)) .| (MktClosing,   t))
              <> ((v .@ Not :< (Products, j, j, Amount)) .| (MktCarryover, t + 1))
    in mconcat [ perFirm kv | kv <- M.toList netMap ]

-- a fixed small (G, A) used by both equivalence tests.
mktBuild :: (HatVal v) => Int -> (TradeNetwork MktFirm, InputCoefficients MktFirm v)
mktBuild n =
    let (gG, gA) = split (mkStdGen 2025)
        fs  = [1 .. n]
        net = erdosRenyi gG fs 0.3
        a   = randomCoefficients gA defaultCoefOptions net
    in (net, a)

mktSpec :: (HatVal v, Real v)
        => Bool -> Int -> Int -> Par -> SimSpec (MktW v) Int MktNote v MktBase
mktSpec tuned n lastT par =
    let fs = [1 .. n] in
    (mkSimSpec (1, lastT) 2025 mkLedger
        [ (if tuned then mktTradeTuned else mktTradeSimple) fs 10
        , mktProduction fs 10
        , mktReport
        , mktCarryover ])
      { Lite.specParallel = par }

mktW0 :: (HatVal v) => Int -> MktW v InitT
mktW0 n = let (net, a) = mktBuild n
          in MktW { mkLedger = carry mempty, mkNet = carry net, mkCoef = carry a }

-- (a) simple ≡ tuned, exactly, under MoneyDecimal (N=30, T=5).
-- the redundant-algebra-correct "same result": net each note's Alg per base
-- ('bar' drops the cancelled part and any zero-padding), keeping the Hat/Not
-- side. simple and tuned differ ONLY in seq redundancy (simple keeps the
-- per-edge posting sequence; tuned pre-sums per base), so they are equal exactly
-- after netting. (norm additivity already holds; this is the stronger per-base
-- exact check.)
nettedMktMap :: Journal MktNote MoneyDecimal MktBase
             -> HM.HashMap MktNote (EA.Alg MoneyDecimal MktBase)
nettedMktMap = toMap . EJ.map EA.bar

testMarketSimpleTunedEqual :: IO ()
testMarketSimpleTunedEqual = do
    let simpleL = runLite (mktSpec False 30 5 Sequential) (mktW0 30) mkLedger
                    :: MktLedgM
        tunedL  = runLite (mktSpec True  30 5 Sequential) (mktW0 30) mkLedger
    assertEqual "Market: tradeStageSimple == tradeStageTuned (MoneyDecimal, exact per-base net)"
        (nettedMktMap simpleL) (nettedMktMap tunedL)
    -- gross volume must also agree: bar-equality alone cannot detect an
    -- accidental early Hat/Not netting in the tuned path (bar is idempotent,
    -- but the pre-bar norm would shrink). norm pins the gross posting volume.
    assertEqual "Market: simple/tuned gross volume (norm) agrees (no early netting)"
        (norm simpleL) (norm tunedL)

-- (b) DET-2: Sequential ≡ ParChunk, exactly, under MoneyDecimal (simple path).
testMarketSeqParEqual :: IO ()
testMarketSeqParEqual = do
    let seqM = runLite (mktSpec False 30 5 Sequential)   (mktW0 30) (toMap . mkLedger)
                 :: HM.HashMap MktNote (EA.Alg MoneyDecimal MktBase)
        parM = runLite (mktSpec False 30 5 (ParChunk 8)) (mktW0 30) (toMap . mkLedger)
    assertEqual "Market DET-2: Sequential == ParChunk (MoneyDecimal, exact)"
        seqM parM

-- (c) sanity: the report's net shortage is strictly positive (Hawkins-Simon),
-- and the complete-network setting also runs (a participating-set sanity).
testMarketShortagePositive :: IO ()
testMarketShortagePositive = do
    let finalSh = runLite (mktSpec False 24 4 Sequential) (mktW0 24)
                    (\final -> norm (EJ.projWithNote [(MktReport, 4)] (mkLedger final)))
                    :: MoneyDecimal
    assertEqual "Market: final-term net shortage is strictly positive (Hawkins-Simon)"
        True (finalSh > 0)
    -- complete network on a tiny N just exercises the dense edge set end-to-end.
    let (gG, gA) = split (mkStdGen 2025)
        cnet     = completeNetwork [1 .. 8 :: MktFirm]
        ccoef    = randomCoefficients gA defaultCoefOptions cnet :: InputCoefficients MktFirm MoneyDecimal
        cw0      = MktW { mkLedger = carry mempty, mkNet = carry cnet, mkCoef = carry ccoef }
        cfs      = [1 .. 8 :: MktFirm]
        cspec    = (mkSimSpec (1, 3) 2025 mkLedger
                      [ mktTradeSimple cfs 10, mktProduction cfs 10, mktReport, mktCarryover ])
        cNorm    = runLite cspec cw0 (norm . mkLedger) :: MoneyDecimal
        _        = gG
    assertEqual "Market: complete-network run produces a positive ledger norm"
        True (cNorm > 0)

-- (d) WINDOW-TRANSPARENCY SENTINEL (Phase 5 fix, modification 3):
-- the carryover bookkeeping makes the model self-contained per term, so a
-- RetainRecent window must NOT change the observable result. Assert that
-- RetainRecent 2 (+ spill) and RetainAll produce the EXACT SAME final-term
-- report norm AND final carryover map (MoneyDecimal, so equality is exact).
-- This permanently guards against the bug this round fixed (a full-ledger
-- inventory sweep silently re-reading a window-truncated net: 9974.74 vs
-- 9993.56). N=40, T=8 so the window (2) is strictly smaller than the history.
testMarketWindowTransparent :: IO ()
testMarketWindowTransparent = withTempSpill "market_window" $ \path -> do
    let n = 40; lastT = 8
        spec = mktSpec False n lastT Sequential
        w0   = mktW0 n
        -- project the two observables we pin: the final-term report norm and the
        -- final carryover map (the next-term opening, keyed by firm).
        project final =
            let lj = mkLedger final :: MktLedgM
                reportN = norm (EJ.projWithNote [(MktReport, lastT)] lj) :: MoneyDecimal
                carryM  = EA.balanceMapBy mktOwnerOfProduct
                            (EJ.toAlg (EJ.projWithNote [(MktCarryover, lastT + 1)] lj))
                          :: M.Map MktFirm MoneyDecimal
            in (reportN, carryM)
        polAll = Policy.defaultLedgerPolicy { Policy.retain = Policy.RetainAll }
        polWin = Policy.defaultLedgerPolicy
                   { Policy.retain = Policy.RetainRecent 2, Policy.spillTo = Just path }
    (allN, allM) <- runLiteWithPolicy polAll spec w0 project
    (winN, winM) <- runLiteWithPolicy polWin spec w0 project
    assertEqual "Market window-transparency: final report norm equal under RetainAll vs RetainRecent 2 + spill"
        allN winN
    assertEqual "Market window-transparency: final carryover map equal under RetainAll vs RetainRecent 2 + spill"
        allM winM

-- (e) stageOf AUTO-NOTE SENTINEL: a 'stageOf' stage and the equivalent manual
-- 'stageFor' that writes @.| (tag, t)@ itself must produce the EXACT SAME ledger
-- (MoneyDecimal, so equality is exact). This pins the semantics of the runner's
-- single auto-attachment of @(stTag, t)@: moving the note from the stage body
-- into 'runStage' changes nothing observable (incl. the zero-drop at the sigma
-- commit). The manual stages below are byte-for-byte the bodies of the migrated
-- mirror stages, but tagged explicitly with the OLD @if isZero then mempty@ form.
mktTradeSimpleManual :: (HatVal v, Real v)
                     => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktTradeSimpleManual fs target = stageFor "trade" fs $ \w t _g j ->
    let net = mkNet w; coef = mkCoef w
        d   = mktDemandOf target t j (mkLedger w)
        sup = suppliersOf net j
        one i = let amt = realToFrac (mktOrderAmt coef d i j)
                in if amt <= 0 then mempty else mktPurchase amt i j
        alg = EA.sigma sup one
    in if EA.isZero alg then mempty else alg .| (MktTrade, t)

mktProductionManual :: (HatVal v, Real v)
                    => [MktFirm] -> Double -> Stage (MktW v) Int MktNote v MktBase
mktProductionManual fs target = stageFor "production" fs $ \w t _g j ->
    let amt = realToFrac (mktDemandOf target t j (mkLedger w))
    in if amt <= 0 then mempty
       else ((amt .@ Hat :< (Products,  j, j, Amount))
          .+ (amt .@ Not :< (SalesCost, j, j, Yen)))
            .| (MktProduction, t)

mktReportManual :: (HatVal v) => Stage (MktW v) Int MktNote v MktBase
mktReportManual = stage "report" $ \w t ->
    let flow = EJ.toAlg (EJ.projWithNote [(MktTrade, t), (MktProduction, t)] (mkLedger w))
        shortageK b = case b of
            Hat :< (Products, o, c, _) | o == c -> Just o
            _                                   -> Nothing
        sh = EA.postFromNetBy shortageK (\j v -> v .@ Not :< (Products, j, j, Amount)) flow
    in if EA.isZero sh then mempty else sh .| (MktReport, t)

-- the same 4-stage spec as 'mktSpec' but with the three single-note stages
-- expressed via manual stageFor + explicit @.| (tag, t)@ (carryover unchanged).
mktSpecManual :: (HatVal v, Real v)
              => Int -> Int -> Par -> SimSpec (MktW v) Int MktNote v MktBase
mktSpecManual n lastT par =
    let fs = [1 .. n] in
    (mkSimSpec (1, lastT) 2025 mkLedger
        [ mktTradeSimpleManual fs 10
        , mktProductionManual fs 10
        , mktReportManual
        , mktCarryover ])
      { Lite.specParallel = par }

testMarketStageOfAutoNote :: IO ()
testMarketStageOfAutoNote = do
    let stageOfL = runLite (mktSpec False 30 5 Sequential) (mktW0 30) (toMap . mkLedger)
                     :: HM.HashMap MktNote (EA.Alg MoneyDecimal MktBase)
        manualL  = runLite (mktSpecManual    30 5 Sequential) (mktW0 30) (toMap . mkLedger)
    assertEqual "Market stageOf auto-note: stageOf ledger == manual stageFor + .| (tag, t) (MoneyDecimal, exact)"
        stageOfL manualL

-- ================================================================
-- ExchangeAlgebra.Optimize: pluggable solvers (Annealing / GA)
-- ================================================================

-- | Shared annealing configuration: minimize over 'Double' with a
--   uniform-step neighbor, geometric cooling and Metropolis acceptance.
optAnnealCfg :: OA.AnnealingConfig Double
optAnnealCfg = OA.AnnealingConfig
    { OA.acDirection = O.Minimize
    , OA.acSteps     = 2000
    , OA.acSchedule  = OA.geometricCooling 1.0 0.995
    , OA.acNeighbor  = \g x -> let (d, g') = randomR (-0.5, 0.5) g in (x + d, g')
    , OA.acAccept    = OA.metropolis
    , OA.acSeed      = 20260717
    }

optSphere :: UV.Vector Double -> Double
optSphere v = UV.sum (UV.map (\x -> (x - 1) ** 2) v)

-- | Annealing locates the minimum of a 1-D quadratic (seed-deterministic),
--   and the reported score is the returned candidate's objective value
--   (score contract: no re-evaluation, user orientation).
testOptimizeAnnealingQuadratic :: IO ()
testOptimizeAnnealingQuadratic = do
    (best, score) <- O.optimize OA.Annealing optAnnealCfg
                         (\x -> return ((x - 3) ** 2)) 0
    assertEqual "Optimize.Annealing: quadratic minimum located (|x-3| < 0.2)"
        True (abs (best - 3) < 0.2)
    assertNear "Optimize.Annealing: score equals returned candidate's value"
        ((best - 3) ** 2) score

-- | GA approaches the sphere minimum, keeps the dimension, and respects
--   per-gene bounds.
testOptimizeGASphere :: IO ()
testOptimizeGASphere = do
    let cfg = OG.defaultGAConfig { OG.gaSeed = 20260717, OG.gaGenerations = 80 }
    (best, score) <- O.optimize OG.GA cfg (return . optSphere) (UV.replicate 3 0)
    assertEqual "Optimize.GA: chromosome dimension preserved" 3 (UV.length best)
    assertEqual "Optimize.GA: sphere minimum approached (score < 0.05)"
        True (score < 0.05)
    let bs     = UV.replicate 3 (-0.5, 0.5)
        bCfg   = OG.defaultGAConfig { OG.gaSeed = 3
                                    , OG.gaGenerations = 40
                                    , OG.gaBounds = Just bs }
    (bBest, _) <- O.optimize OG.GA bCfg (return . optSphere) (UV.replicate 3 0)
    assertEqual "Optimize.GA: bounds respected by every gene"
        True (UV.all (\x -> x >= -0.5 && x <= 0.5) bBest)

-- | Maximize reports the score in the user's orientation.
testOptimizeGAMaximize :: IO ()
testOptimizeGAMaximize = do
    let cfg = OG.defaultGAConfig { OG.gaDirection = O.Maximize
                                 , OG.gaSeed = 7
                                 , OG.gaGenerations = 80 }
    (_, score) <- O.optimize OG.GA cfg (return . negate . optSphere)
                      (UV.replicate 3 0)
    assertEqual "Optimize.GA: Maximize reports user-oriented score (> -0.05)"
        True (score > -0.05)

-- | Acceptance-criteria demo: the objective runs in @ST s@ (mutable
--   evaluation counter), and the solver evaluates exactly once per step
--   plus once for the initial candidate.
testOptimizeSTObjective :: IO ()
testOptimizeSTObjective = do
    let cfg = optAnnealCfg { OA.acSteps = 100 }
        (best, score, evals) = runST $ do
            ref    <- newSTRef (0 :: Int)
            (b, s) <- O.optimize OA.Annealing cfg
                          (\x -> modifySTRef' ref (+ 1) >> return ((x - 3) ** 2)) 0
            n      <- readSTRef ref
            return (b, s, n)
    assertEqual "Optimize: ST objective evaluated once per step + initial"
        (OA.acSteps cfg + 1) evals
    assertEqual "Optimize: ST run returns finite (best, score)"
        True (not (isNaN best) && not (isNaN score) && not (isInfinite score))

-- | Same seed, same (deterministic) objective => identical result.
testOptimizeDeterminism :: IO ()
testOptimizeDeterminism = do
    r1 <- O.optimize OA.Annealing optAnnealCfg (\x -> return ((x - 3) ** 2)) 0
    r2 <- O.optimize OA.Annealing optAnnealCfg (\x -> return ((x - 3) ** 2)) 0
    assertEqual "Optimize.Annealing: same seed => identical result" r1 r2
    let cfg = OG.defaultGAConfig { OG.gaSeed = 11, OG.gaGenerations = 20 }
    s1 <- O.optimize OG.GA cfg (return . optSphere) (UV.replicate 3 0)
    s2 <- O.optimize OG.GA cfg (return . optSphere) (UV.replicate 3 0)
    assertEqual "Optimize.GA: same seed => identical result" s1 s2

-- | Fail-fast contract: non-finite objective scores and invalid
--   configurations are rejected with 'error', never silently absorbed.
testOptimizeFailFast :: IO ()
testOptimizeFailFast = do
    r <- try (O.optimize OA.Annealing optAnnealCfg (\_ -> return (0 / 0)) 0)
    case (r :: Either SomeException (Double, Double)) of
        Left _  -> putStrLn "[PASS] Optimize.Annealing: NaN objective rejected (fail-fast)"
        Right v -> do
            putStrLn ("[FAIL] Optimize.Annealing: NaN objective accepted: " ++ show v)
            exitFailure
    let badCfg = OG.defaultGAConfig { OG.gaEliteCount = 999 }
    r2 <- try (O.optimize OG.GA badCfg (return . optSphere) (UV.replicate 3 0))
    case (r2 :: Either SomeException (UV.Vector Double, Double)) of
        Left _  -> putStrLn "[PASS] Optimize.GA: invalid config rejected (fail-fast)"
        Right v -> do
            putStrLn ("[FAIL] Optimize.GA: invalid config accepted: " ++ show v)
            exitFailure

main :: IO ()
main = do
    testAccountTitlesBinary
    testAccountTitleClassification
    testProjMultiPatternOnePass
    testProjNormFastPath
    testProjDuplicateExact
    testProjExactWildcardOverlap
    testProjNormBarIdentity
    testProjWithBaseNorm
    testProjWithNoteNorm
    testProjWithBaseNormBothSided
    testBasesNotSideRegression
    testNumericToleranceScaleAware
    testMoneyDecimalExactOrderIndependent
    testSigmaMergePath
    testSameBaseSeqOrderPathDependence
    testSigma2When
    testSigmaFromMap
    testJournalFromListStrict
    testUnionZeroSingletonBase
    testScalarRejectsNegative
    testProjConcreteNoIndexForce
    testLinerReservedFieldsPoisoned
    testJournalSigmaMergePath
    testJournalSigma2When
    testJournalSigmaOn
    testJournalSigmaOnFromMap
    testFilterByAxisEquivalent
    testFilterByAxisWithDeltaUpdates
    testFinalStockTransferAlgEquivalence
    testFinalStockTransferJournalEquivalence
    testFinalStockRegistryClosedDiff
    testFinalStockRuleReference
    testVocabOrdinalPin
    testPreVland2SemanticsClosedDiff
    testIncomeSummaryBalancedNoCrash
    testSpillDecisionSingleSource
    testRestoreJournalFromBinarySpill
    testSimulateEx1Default
    testCsvTranspose
    testCsvWriteCSV
    testCsvWriteCSVWithQuotes
    testCsvWriteCSVEmpty
    testWriteBSPinned
    testWritePLPinned
    testWriteJournalPinned
    testWriteCompoundTrialBalancePinned
    testWriteAccountOfJournalPinned
    testLiteBoilerplate
    testLiteDet2
    testLiteDet1
    testLiteBspInvisibility
    testLiteGateEquivalence
    testLiteFieldRules
    testLiteBoundaryOncePerTerm
    testPolicyEquivalence
    testPolicyWindowRoundTrip
    testPolicyCompressClosed
    testPolicyDeleteOnly
    testPolicyDeterminism
    testPolicyClassicBridge
    testPolicyHasTermAxis
    testNetCompleteEquiv
    testNetDeterminism
    testNetSmartConstructor
    testNetHawkinsSimon
    testNetGeneratorStructure
    testNetAdjacencyConsistency
    testNetCsvRoundTrip
    testMarketSimpleTunedEqual
    testMarketSeqParEqual
    testMarketShortagePositive
    testMarketWindowTransparent
    testMarketStageOfAutoNote
    testConvertCsvRoundTrip
    testAssistDescriptionsDrift
    testAssistDescribeAccount
    testAssistAllAccountInfos
    testAccountMetadataLand1
    testAccountMetadataLand1Golden
    testAccountInfoLand1Migration
    testAssistSuggestAccounts
    testPostVocabGolden
    testAccountSemanticsPrechangeGolden
    testRegistryGolden
    testJcciAccountNameCoverage
    testRegistryWildcards
    testRegistryContraLand2
    testLand2SemanticsClosedDiff
    testLand2InfoClosedDiff
    testLand2SuggestClosedDiff
    testLand2Contract
    testLand2PimoFlip
    testLand2ExchangeRelation
    testLand2IsContraInstances
    testLand2AiDivision
    testLand2Presentation
    testLand2PresentationClosedDiff
    testLand2HatNotPolicy
    checkedConvertProperties
    axiomProperties
    journalProperties
    quotientProperties
    bookkeepingProperties
    closingDocsTests
    testOptimizeAnnealingQuadratic
    testOptimizeGASphere
    testOptimizeGAMaximize
    testOptimizeSTObjective
    testOptimizeDeterminism
    testOptimizeFailFast
