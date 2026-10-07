-- | Account semantics, registries, statements, and accounting fixtures.
-- Tests exercise the Accounting layer using the shared Support fixtures.
-- Start with 'runTests' for the suite's execution order.
module Accounting.RegistrySpec (runTests) where

import ExchangeAlgebra.Journal
import qualified ExchangeAlgebra.IO.Input      as EC
import qualified ExchangeAlgebra.Accounting.Account as PP
import qualified ExchangeAlgebra.Accounting.Consolidation as CW
import qualified ExchangeAlgebra.Accounting.TrialBalance.Balance as TBB
import qualified ExchangeAlgebra.Accounting.TrialBalance.Validation as TB
import qualified ExchangeAlgebra.Accounting.Statements.Presentation as RP
import qualified ExchangeAlgebra.Accounting.Statements.Metric as RM
import qualified ExchangeAlgebra.Accounting.Statements.Group as RG
import qualified ExchangeAlgebra.IO.Input.Assist       as Assist
import qualified ExchangeAlgebra.Accounting.Account as Registry
import qualified ExchangeAlgebra.Algebra  as EA
import qualified ExchangeAlgebra.Algebra.Transfer as EAT
import qualified ExchangeAlgebra.Journal  as EJ
import qualified ExchangeAlgebra.Journal.Transfer as EJT
import qualified ExchangeAlgebra.Accounting.Entries as EB
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.Write    as EW
import ExchangeAlgebra.Write (bsRows, plRows)
import qualified Data.Map.Strict     as M
import qualified Data.List           as L
import qualified Data.List.NonEmpty  as NE
import qualified Data.Set            as Set
import Data.Char (isAlpha, isAscii)
import qualified Data.Binary         as Binary
import qualified Data.Binary.Put     as BinaryPut
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import Golden.WriteRows (writeRowsFixtureDir, writeRowsFixtures)
import qualified Golden.ReadoutBaseline as ReadoutBaseline
import qualified Golden.ExampleNumbers.RippleFixture as RippleFixture
import qualified Golden.AdmissionBaseline as AdmissionBaseline
import System.Environment (lookupEnv)
import Numeric (showHex)
import Control.Monad (forM_)
import System.Exit (exitFailure)
import Data.Time (Day, TimeOfDay(..), fromGregorian)
import Control.Exception (try, evaluate, SomeException)
import Test.QuickCheck hiding (Fixed)
import Support (assertEqual
               , assertNear
               , TransferAlg
               , TransferJournal
               , SimHatBase2
               , quickProp
               , genNNDouble
               , epsEq
               , CheckedAlgM
               , exactBalancedForTest
               )

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
    assertEqual "AccountTitles Binary covers all 243 constructors"
        243 (L.length titles)
    assertEqual "AccountTitles Binary Word16be roundtrip"
        titles roundTripped
    assertEqual "AccountTitles Binary rejects out-of-range Word16"
        True (case decodeAccountTitleOrFail invalidBytes of
            Left _  -> True
            Right _ -> False)

-- | Check the appended allowances and the additional Japanese aliases.
testAllowanceAccountTitles :: IO ()
testAllowanceAccountTitles = do
    let titles = [SalesAllowances, PurchaseAllowances]
    assertEqual "allowance account titles Binary roundtrip"
        titles (L.map (Binary.decode . Binary.encode) titles)
    assertEqual "allowance account titles division"
        [Revenue, Cost] (L.map classifyAccountDivision titles)
    assertEqual "allowance account titles home side"
        [Debit, Credit]
        [whichSide (Not :< title :: HatBase AccountTitles) | title <- titles]
    assertEqual "allowance names and additional Japanese aliases parse"
        (L.map Right [SalesAllowances, PurchaseAllowances, DeliveryExpenses, Fixtures])
        (L.map EC.parseAccountTitle (L.map T.pack
            ["売上値引", "仕入値引", "荷造運賃", "器具備品"]))

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
    , (ConsumptionTaxRefundReceivable, Assets, Debit, Current)
    , (PropertyTaxPayable, Liability, Credit, Current)
    , (DepositsReceivedFromOfficers, Liability, Credit, Current)
    , (EntertainmentExpenses, Cost, Debit, Other)
    , (MeetingExpenses, Cost, Debit, Other)
    , (NewspaperBooksExpenses, Cost, Debit, Other)
    , (RawMaterials, Assets, Debit, Current)
    , (GoodsInTransit, Assets, Debit, Current)
    , (SalesAllowances, Revenue, Debit, Other)
    , (PurchaseAllowances, Cost, Credit, Other)
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


type FinalStockProbe = EA.Alg Double (HatBase AccountTitles)

-- | Classify the observable image of a one-posting probe.
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

-- ================================================================
-- V-Land 2 scaffolding (語彙拡張の受理条件, レビュー非依存):
-- pre-vland2 fixture (tools/DumpVocabGolden.hs で生成, commit 85d6a7f に pin)
-- に対する Enum 挿入規律 pin と意味関数 closed-diff。
-- ================================================================

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
    assertEqual "vocab ordinal pin: appended constructor ordinals and wildcard"
        [232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242]
        (L.map fromEnum
            [ ConsumptionTaxRefundReceivable
            , PropertyTaxPayable
            , DepositsReceivedFromOfficers
            , EntertainmentExpenses
            , MeetingExpenses
            , NewspaperBooksExpenses
            , RawMaterials
            , GoodsInTransit
            , SalesAllowances
            , PurchaseAllowances
            , AccountTitle
            ])
    -- concreteAccountTitles は wildcard 以外の全 constructor を被覆すること。
    -- 現行の hardcoded 上限 ([Cash .. ReversalOfAllowanceForDoubtfulAccounts]) は
    -- 挿入後に新規科目が漏れるため, この assert が V-Land 2 に
    -- filter (/= wildcard) [minBound ..] への導出化を強制する。
    assertEqual "vocab ordinal pin: concreteAccountTitles covers all non-wildcard constructors"
        (L.filter (/= (AccountTitle :: AccountTitles)) [minBound .. maxBound])
        Registry.concreteAccountTitles

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


testAssistAllAccountInfos :: IO ()
testAssistAllAccountInfos = do
    assertEqual "Assist.allAccountInfos length" 242 (length Assist.allAccountInfos)
    assertEqual "Assist.allAccountInfos follows concreteAccountTitles order"
        PP.concreteAccountTitles (L.map Assist.aiTitle Assist.allAccountInfos)
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

testAccountSemanticsRegistryInvariants :: IO ()
testAccountSemanticsRegistryInvariants = do
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
    assertEqual "metadata covers all 242 concrete titles"
        242 (L.length semantics)
    assertEqual "metadata rejects wildcard AccountTitle"
        Nothing (Registry.accountSemantics AccountTitle)
    assertEqual "non-statement metadata is exactly the reviewed exception set"
        (L.sort exceptional) (L.sort nonStatementTitles)
    forM_ semantics $ \(title, value) -> do
        assertEqual ("roles are non-empty: " ++ show title)
            True (not (L.null (Registry.asemRoles value)))
        -- Land 4a: rolesFor no longer consults asIsContra (explicit
        -- enumeration), so pin the contra role to the registry flag.
        assertEqual ("contra role matches registry isContra: " ++ show title)
            (Registry.classifyAccountContra title)
            (ContraAccount `elem` Registry.asemRoles value)
        case Registry.asemDivisionSemantics value of
            StatementDivision division -> do
                assertEqual ("statement division preserves legacy value: " ++ show title)
                    (classifyAccountDivision title) division
            _ -> assertEqual ("exceptional title is closed-listed: " ++ show title)
                    True (title `L.elem` exceptional)
        case Registry.asemHomeSideSemantics value of
            FixedHomeSide side ->
                assertEqual ("fixed home side preserves legacy value: " ++ show title)
                    (whichSide (Not :< title)) side
            _ -> pure ()
    assertEqual "Cash semantics"
        (Just ( [OrdinaryAccount], OrdinaryPosting
              , StatementDivision Assets, FixedHomeSide Debit, StatementEligible ))
        (fmap semanticsTuple (lookupSem Cash))
    assertEqual "IncomeSummary semantics"
        (Just ( [ClosingDevice], ClosingOnly
              , DirectionEncoding Assets, ContextDependentHomeSide, NotPresented ))
        (fmap semanticsTuple (lookupSem IncomeSummary))
    assertEqual "NetIncome semantics"
        (Just ( [PeriodResult], EngineGeneratedOnly
              , DirectionEncoding Cost, FixedHomeSide Debit, DerivedPresentation ))
        (fmap semanticsTuple (lookupSem NetIncome))
    assertEqual "GrossProfit is an engine-generated coordinate"
        (Just ( [ReportingSubtotal], EngineGeneratedOnly
              , DirectionEncoding Revenue, FixedHomeSide Credit
              , DerivedPresentation ))
        (fmap semanticsTuple (lookupSem GrossProfit))
    assertEqual "NCI profit is distinct from bare net income"
        (Just ( [AttributionAccount, PeriodResult], ConsolidationOnly
              , DirectionEncoding Cost, FixedHomeSide Debit
              , ContextualPresentation ))
        (fmap semanticsTuple (lookupSem NetIncomeAttributableToNCI))
    assertEqual "NCI equity is consolidation-only"
        (Just ( [AttributionAccount], ConsolidationOnly
              , StatementDivision Equity, FixedHomeSide Credit
              , ContextualPresentation ))
        (fmap semanticsTuple (lookupSem NonControllingInterests))
    assertEqual "branch account semantics"
        (Just ( [ReciprocalAccount], OrdinaryPosting
              , BookkeepingControlClass Assets, FixedHomeSide Debit
              , ContextualPresentation ))
        (fmap semanticsTuple (lookupSem BranchCurrentAccount))
    assertEqual "IncomeSummary LLM description does not classify it as an asset"
        True (case lookupInfo IncomeSummary of
            Just info -> not (T.isPrefixOf (T.pack "Asset") (Assist.aiDesc info))
                      && T.isInfixOf (T.pack "not a balance-sheet classification")
                                     (Assist.aiDesc info)
            Nothing -> False)
    assertEqual "NetIncome LLM name drops legacy Expense wording"
        (Just (T.pack "当期純利益")) (fmap Assist.aiNameJa (lookupInfo NetIncome))
  where
    semanticsTuple value =
        ( Registry.asemRoles value
        , Registry.asemPostingCapability value
        , Registry.asemDivisionSemantics value
        , Registry.asemHomeSideSemantics value
        , Registry.asemReportingEligibility value
        )

accountRegistryHeader :: T.Text -> T.Text
accountRegistryHeader what =
    T.pack "# account-semantics-050 Land 1 " <> what
    <> T.pack "; schema 1; base 09c8a60c0bfb1a7fedb01689ceee789b8b4e6084\n"

accountRegistryRow :: AccountTitles -> T.Text
accountRegistryRow title = case Registry.accountSemantics title of
    Nothing -> error ("missing AccountSemantics for " ++ show title)
    Just semantics -> T.intercalate (T.pack "\t")
        [ goldenShow title
        , goldenShow (Registry.asemRoles semantics)
        , goldenShow (Registry.asemPostingCapability semantics)
        , goldenShow (Registry.asemDivisionSemantics semantics)
        , goldenShow (Registry.asemHomeSideSemantics semantics)
        , goldenShow (Registry.asemReportingEligibility semantics)
        ]

accountRegistryInfoRow :: Assist.AccountInfo -> T.Text
accountRegistryInfoRow info = T.intercalate (T.pack "\t")
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

accountRegistrySuggestions :: T.Text
accountRegistrySuggestions =
    accountRegistryHeader
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

testAccountSemanticsGolden :: IO ()
testAccountSemanticsGolden = do
    metadata <- TIO.readFile "test/fixtures/account-semantics-050/metadata.tsv"
    info <- TIO.readFile "test/fixtures/account-semantics-050/account-info.tsv"
    suggest <- TIO.readFile "test/fixtures/account-semantics-050/suggest.tsv"
    let expectedMetadata =
            accountRegistryHeader
                (T.pack "registry (title, roles, posting, divisionSemantics, homeSideSemantics, reportingEligibility)")
            <> T.unlines (L.map accountRegistryRow Registry.concreteAccountTitles)
        expectedInfo =
            accountRegistryHeader
                (T.pack "LLM AccountInfo (title, roles, posting, divisionSemantics, homeSideSemantics, reportingEligibility, nameEn, nameJa, description)")
            <> T.unlines (L.map accountRegistryInfoRow Assist.allAccountInfos)
    assertEqual "metadata fixture has 242 rows"
        242 (L.length (L.drop 1 (T.lines metadata)))
    assertEqual "metadata fixture" metadata expectedMetadata
    assertEqual "LLM AccountInfo fixture" info expectedInfo
    assertEqual "LLM suggestion fixture"
        suggest accountRegistrySuggestions

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
-- Registry goldens: alias resolution and account behavior.
-- ================================================================

goldenHeader :: T.Text -> T.Text
goldenHeader what = T.pack "# registry-aliases " <> what <> T.pack "; schema 1\n"

-- | Compare a rendered golden with its fixture. @EA_REGEN_GOLDEN=1@ rewrites
-- the fixture from the current output instead.
goldenCheck :: String -> FilePath -> T.Text -> IO ()
goldenCheck label path expected = do
    regen <- lookupEnv "EA_REGEN_GOLDEN"
    case regen of
        Just "1" -> TIO.writeFile path expected
        _ -> do
            actual <- TIO.readFile path
            assertEqual label actual expected

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
-- pinned separately by accountRegistryInfoRow.
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
-- Account behavior over every concrete title: closing, projection membership,
-- and the one-posting statement rows.
-- ================================================================

accountSemanticsHeader :: T.Text -> T.Text
accountSemanticsHeader what =
    T.pack "# account-algebra-behavior " <> what <> T.pack "; schema 1\n"

accountSemanticsBinaryHex :: AccountTitles -> T.Text
accountSemanticsBinaryHex = T.pack . concatMap hexByte . BL.unpack . Binary.encode
  where
    hexByte byte = case showHex byte "" of
        [digit] -> ['0', digit]
        digits  -> digits

accountSemanticsBaselineTitles :: [AccountTitles]
accountSemanticsBaselineTitles = Registry.concreteAccountTitles

accountSemanticsSemanticsGolden :: T.Text
accountSemanticsSemanticsGolden =
    accountSemanticsHeader (T.pack "semantics (title, enum, binaryHex, division, closing, isContra, whichSide Not, whichSide Hat, whatPIMO, fixedCurrent, finalStockProbe)")
    <> T.unlines (L.map row accountSemanticsBaselineTitles)
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

accountSemanticsProjectionGolden :: T.Text
accountSemanticsProjectionGolden =
    accountSemanticsHeader (T.pack "projection flags for Not then Hat (currentAssets, fixedAssets, deferredAssets, currentLiability, fixedLiability, capitalStock, contraAssets, contra)")
    <> T.unlines (L.map row accountSemanticsBaselineTitles)
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
    <> T.unlines (L.map row accountSemanticsBaselineTitles)
  where
    row title =
        let value = 1 .@ Not :< title :: EA.Alg Double (HatBase AccountTitles)
        in T.intercalate (T.pack "\t")
            [ goldenShow title
            , goldenEsc (goldenShow (EW.bsRows value))
            , goldenEsc (goldenShow (EW.plRows value))
            ]

testAccountAlgebraBehaviorGolden :: IO ()
testAccountAlgebraBehaviorGolden = do
    let dir = "test/fixtures/account-algebra-behavior/"
    goldenCheck "account algebra behavior: closing and semantics"
        (dir ++ "semantics.tsv") accountSemanticsSemanticsGolden
    goldenCheck "account algebra behavior: projection membership"
        (dir ++ "projection-membership.tsv") accountSemanticsProjectionGolden
    goldenCheck "account algebra behavior: one-posting statement rows"
        (dir ++ "presentation.tsv") accountSemanticsPresentationGolden
    semantics <- TIO.readFile (dir ++ "semantics.tsv")
    assertEqual "account algebra behavior covers every concrete title"
        (L.length Registry.concreteAccountTitles)
        (L.length (L.filter (not . T.null) (L.drop 1 (T.lines semantics))))

testWriteRowsGolden :: IO ()
testWriteRowsGolden = do
    assertEqual "write-rows-0510 fixture count"
        10 (L.length writeRowsFixtures)
    forM_ writeRowsFixtures $ \(name, expected) -> do
        actualFile <- TIO.readFile (writeRowsFixtureDir ++ "/" ++ name)
        assertEqual ("write-rows-0510 fixture " ++ name) actualFile expected

testReadoutBaselineGolden :: IO ()
testReadoutBaselineGolden = do
    assertEqual "readout-baseline-p1 fixture count"
        21 (L.length fixtures)
    regen <- lookupEnv "EA_REGEN_GOLDEN"
    forM_ fixtures $ \(name, expected) -> do
        let path = ReadoutBaseline.readoutFixtureDir ++ "/" ++ name
        case regen of
            Just "1" -> TIO.writeFile path expected
            _ -> do
                actualFile <- TIO.readFile path
                assertEqual ("readout-baseline-p1 fixture " ++ name) actualFile expected
  where
    fixtures = ReadoutBaseline.readoutFixtures ++ ReadoutBaseline.keyedFixtures

testExampleNumbersGolden :: IO ()
testExampleNumbersGolden = do
    expected <- RippleFixture.renderFixture
    let path = RippleFixture.fixtureDir ++ "/" ++ RippleFixture.fixtureName
        numericRows = L.length (T.lines expected) - 1
    assertEqual "example-numbers-p1 ripple numeric row count" 243 numericRows
    regen <- lookupEnv "EA_REGEN_GOLDEN"
    case regen of
        Just "1" -> TIO.writeFile path expected
        _ -> do
            actual <- TIO.readFile path
            assertEqual "example-numbers-p1 ripple fixture" actual expected

testAdmissionBaselineGolden :: IO ()
testAdmissionBaselineGolden = do
    assertEqual "admission-baseline-p1 fixture count"
        3 (L.length AdmissionBaseline.admissionFixtures)
    regen <- lookupEnv "EA_REGEN_GOLDEN"
    forM_ AdmissionBaseline.admissionFixtures $ \(name, expected, caseCount) -> do
        let path = AdmissionBaseline.admissionFixtureDir ++ "/" ++ name
            expectedCount = case name of
                "boundary.tsv" -> 64
                "equivalence.tsv" -> 12
                "catalog.tsv" -> 264
                _ -> 0
        assertEqual ("admission-baseline-p1 case count " ++ name)
            expectedCount caseCount
        assertEqual ("admission-baseline-p1 rendered rows " ++ name)
            expectedCount (L.length (T.lines expected) - 2)
        case regen of
            Just "1" -> TIO.writeFile path expected
            _ -> do
                actual <- TIO.readFile path
                assertEqual ("admission-baseline-p1 fixture " ++ name) actual expected

-- | Every historical alias query resolves as pinned. The queries come from the
-- fixture itself; regenerate with @EA_REGEN_GOLDEN=1@ after an intended change.
testAliasResolutionGolden :: IO ()
testAliasResolutionGolden = do
    let path = "test/fixtures/registry-aliases/alias-resolution.tsv"
    aliases <- TIO.readFile path
    assertEqual "alias resolution golden: query count"
        703 (L.length (L.filter (not . T.null) (L.drop 1 (T.lines aliases))))
    goldenCheck "alias resolution golden" path (goldenAliasResolution aliases)

-- | JCCI 2022 A欄/B欄の全 distinct query は, 一意のRightかfixtureで候補を
-- 閉じたAmbiguousのどちらかでなければならない. Unknown/first-matchは不可.
testJcciAccountNameCoverage :: IO ()
testJcciAccountNameCoverage = do
    assertEqual "new account titles parse Japanese names and material alias"
        (L.map Right
            [ EntertainmentExpenses
            , MeetingExpenses
            , NewspaperBooksExpenses
            , RawMaterials
            , GoodsInTransit
            , RawMaterials
            ])
        (L.map EC.parseAccountTitle (L.map T.pack
            [ "交際費", "会議費", "新聞図書費", "原材料", "未着品", "材料" ]))
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
            in case traverse (`M.lookup` statementTitleMap) names of
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

-- | Every cleaned Japanese label is a bare account name, and every level-2
-- JCCI A-column name that resolves uniquely is the profile display label.
testJapaneseAccountLabels :: IO ()
testJapaneseAccountLabels = do
    let labels =
            [ (title, Registry.asLabelJa spec)
            | title <- Registry.concreteAccountTitles
            , Just spec <- [Registry.accountSpec title]
            ]
        forbidden = L.map T.pack ["。", "—", "\\/", "'"]
        invalid =
            [ (title, label)
            | (title, label) <- labels
            , any (`T.isInfixOf` label) forbidden
                || T.any (\c -> isAscii c && isAlpha c) label
            ]
    assertEqual "asLabelJa covers all 242 concrete titles"
        242 (L.length labels)
    assertEqual "asLabelJa contains only cleaned Japanese account names"
        ([] :: [(AccountTitles, T.Text)]) invalid

    source <- TIO.readFile "test/fixtures/jcci-2022/source.tsv"
    let rows =
            [ fields
            | row <- L.drop 1 (T.lines source)
            , not (T.null row)
            , let fields = T.splitOn (T.pack "\t") row
            , L.length fields == 6
            , T.pack "2" `T.isInfixOf` (fields L.!! 0)
            ]
        aNames = L.map (L.!! 2) rows
        resolved =
            [ (name, title, RP.presentationLabel RP.JcciSecondGradeReport title)
            | name <- aNames
            , Right title <- [EC.parseAccountTitle name]
            ]
        skipped =
            [ name
            | name <- aNames
            , Left (EC.AmbiguousAccount _ _) <- [EC.parseAccountTitle name]
            ]
        unexpected =
            [ (name, show err)
            | name <- aNames
            , Left err <- [EC.parseAccountTitle name]
            , case err of EC.AmbiguousAccount _ _ -> False; _ -> True
            ]
        mismatches =
            [ (name, title, label)
            | (name, title, label) <- resolved
            , label /= name
            ]
    assertEqual "JCCI level-2 A-column rows" 117 (L.length aNames)
    assertEqual "JCCI A-column MATCH count" 113 (L.length resolved)
    assertEqual "JCCI A-column skip set"
        (L.sort (L.map T.pack
            ["営業収益", "営業費用", "為替差損益", "有価証券評価損益"]))
        (L.sort skipped)
    assertEqual "JCCI A-column has no unexpected parse failures"
        ([] :: [(T.Text, String)]) unexpected
    assertEqual "JCCI A-column labels MATCH 113/113"
        ([] :: [(T.Text, AccountTitles, T.Text)]) mismatches

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


-- ================================================================
-- Contra accounts (Definition 7 contra amendment): contract and relation
-- properties, presentation invariance.
-- ================================================================

contraAssetTitles :: [AccountTitles]
contraAssetTitles = [AllowanceForDoubtfulAccounts, AccumulatedDepreciation]

allContra :: [AccountTitles]
allContra = contraAssetTitles
    <> [SalesRebates, RefundOfIncomeTaxes, PurchaseRebates, SalesAllowances, PurchaseAllowances]

statementTitleMap :: M.Map T.Text AccountTitles
statementTitleMap = M.fromList
    [ (T.pack (show t), t) | t <- Registry.concreteAccountTitles ]

-- T2: 契約 isContra(b) ⇔ homeSide(b) ≠ defaultSide(whatDiv b)
testContraIffReversedHomeSide :: IO ()
testContraIffReversedHomeSide = mapM_ check Registry.concreteAccountTitles
  where
    check t =
        let nb = Not :< t :: HatBase AccountTitles
        in assertEqual ("contract (isContra = reversed home side): " ++ show t)
            (isContra nb) (whichSide nb /= defaultSide (whatDiv nb))

-- T3: pimoFlip は自己逆で, 原典の交換関係を保つ
testPimoFlipInvolution :: IO ()
testPimoFlipInvolution = do
    let pimoAll = [PS, IN, MS, OUT]
    mapM_ (\x -> assertEqual ("pimoFlip involution: " ++ show x)
              x (pimoFlip (pimoFlip x))) pimoAll
    mapM_ (\(x, y) -> assertEqual ("pimoFlip preserves (<=>): " ++ show (x, y))
              (x <=> y) (pimoFlip x <=> pimoFlip y))
          [ (x, y) | x <- pimoAll, y <- pimoAll ]

-- | Check the PIMO relation and its account-division extension (Prop 5.3.8).
testExchangeRelationProp538 :: IO ()
testExchangeRelationProp538 = do
    let pimoAll = [PS, IN, MS, OUT]
        divAll  = [Assets, Equity, Liability, Cost, Revenue]
    assertEqual "(<=>) PIMO instance = Prop 5.3.8 pairs"
        [ (PS,IN), (PS,MS), (IN,PS), (IN,OUT), (MS,PS), (MS,OUT), (OUT,IN), (OUT,MS) ]
        [ (x, y) | x <- pimoAll, y <- pimoAll, x <=> y ]
    mapM_ (\(a, b) -> assertEqual ("(<=>) division = via pimoFromDivision: " ++ show (a, b))
              (pimoFromDivision a <=> pimoFromDivision b) (a <=> b))
          [ (a, b) | a <- divAll, b <- divAll ]


-- T9: 8 組込み instance + custom instance (SimHatBase2) の全科目 sweepで
-- isContra のTrue集合が既存評価勘定2件 + V-Land 2 P/L控除3件になること。
testIsContraSweepAcrossBaseInstances :: IO ()
testIsContraSweepAcrossBaseInstances = do
    let day0 = fromGregorian 2026 1 1
        tod0 = TimeOfDay 0 0 0
        nm   = T.pack "spec"
        sweep :: ExBaseClass b => String -> (AccountTitles -> b) -> IO ()
        sweep label mk = assertEqual ("isContra sweep: " ++ label)
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


testPresentationGroups :: IO ()
testPresentationGroups = do
    let defaultDefs = RG.defaultPresentationGrouping
        tradeDef = maybe (error "missing TradeReceivablesGroup") id
            (RG.lookupGroupDef defaultDefs RG.TradeReceivablesGroup)
        amount below magnitude = RG.RelativeAmount below (magnitude :: Double)
        present defs entries = RG.presentGroups defs (M.fromList entries)
        contraTitles =
            [ title
            | title <- Registry.concreteAccountTitles
            , maybe False Registry.asIsContra (Registry.accountSpec title)
            ]
        deductionTitles = L.concatMap RG.pgDeductions defaultDefs
        allMembers def = RG.pgGross def ++ RG.pgDeductions def
    assertEqual "default groups cover each registry contra exactly once"
        (L.sort contraTitles) (L.sort deductionTitles)
    assertEqual "net sales group includes sales allowances as a deduction"
        (Just [SalesRebates, SalesAllowances])
        (RG.pgDeductions <$> RG.lookupGroupDef defaultDefs RG.NetSalesGroup)
    assertEqual "net purchases group includes purchase allowances as a deduction"
        (Just [PurchaseRebates, PurchaseAllowances])
        (RG.pgDeductions <$> RG.lookupGroupDef defaultDefs RG.NetPurchasesGroup)
    assertEqual "default group memberships are disjoint"
        (L.length (L.concatMap allMembers defaultDefs))
        (Set.size (Set.fromList (L.concatMap allMembers defaultDefs)))
    forM_ defaultDefs $ \def ->
        forM_ (allMembers def) $ \title ->
            assertEqual ("default title lookup: " ++ show title)
                (Just (RG.pgKey def)) (RG.presentationGroupOf title)
    forM_ defaultDefs $ \def -> do
        forM_ (RG.pgGross def) $ \title ->
            assertEqual ("gross member is non-contra: " ++ show title)
                (Just (RG.pgDivision def, False))
                ((\spec -> (Registry.asDivision spec, Registry.asIsContra spec))
                    <$> Registry.accountSpec title)
        forM_ (RG.pgDeductions def) $ \title ->
            assertEqual ("deduction member is same-division contra: " ++ show title)
                (Just (RG.pgDivision def, True))
                ((\spec -> (Registry.asDivision spec, Registry.asIsContra spec))
                    <$> Registry.accountSpec title)

    let exceeded = present [tradeDef]
            [ (AccountsReceivable, (100, 0))
            , (AllowanceForDoubtfulAccounts, (0, 150))
            ]
    assertEqual "edge: contra exceeding gross yields a negative net"
        [(tradeDef,
            [ RG.GroupRow (RG.GrossRow AccountsReceivable) (amount False 100)
            , RG.GroupRow (RG.DeductionRow AllowanceForDoubtfulAccounts) (amount True 150)
            , RG.GroupRow (RG.NetRow RG.TradeReceivablesGroup) (amount True 50)
            ])]
        (RG.gpBlocks exceeded)

    let parentAbsent = present [tradeDef]
            [(AllowanceForDoubtfulAccounts, (0, 30))]
    assertEqual "edge: parent absent still renders deduction and net"
        [(tradeDef,
            [ RG.GroupRow (RG.DeductionRow AllowanceForDoubtfulAccounts) (amount True 30)
            , RG.GroupRow (RG.NetRow RG.TradeReceivablesGroup) (amount True 30)
            ])]
        (RG.gpBlocks parentAbsent)

    let multiContraDef = tradeDef
            { RG.pgGross = [AccountsReceivable]
            , RG.pgDeductions =
                [AllowanceForDoubtfulAccounts, AccumulatedDepreciation]
            }
        multipleContra = present [multiContraDef]
            [ (AccountsReceivable, (1000, 0))
            , (AllowanceForDoubtfulAccounts, (0, 100))
            , (AccumulatedDepreciation, (0, 200))
            ]
    assertEqual "edge: multiple contra rows deduct exactly once"
        [(multiContraDef,
            [ RG.GroupRow (RG.GrossRow AccountsReceivable) (amount False 1000)
            , RG.GroupRow (RG.DeductionRow AllowanceForDoubtfulAccounts) (amount True 100)
            , RG.GroupRow (RG.DeductionRow AccumulatedDepreciation) (amount True 200)
            , RG.GroupRow (RG.NetRow RG.TradeReceivablesGroup) (amount False 700)
            ])]
        (RG.gpBlocks multipleContra)

    let child = tradeDef { RG.pgParent = Just RG.DepreciableAssetsGroup }
        parent = maybe (error "missing DepreciableAssetsGroup") id
            (RG.lookupGroupDef defaultDefs RG.DepreciableAssetsGroup)
        nested = present [parent, child]
            [ (AccountsReceivable, (1000, 0))
            , (AllowanceForDoubtfulAccounts, (0, 100))
            , (Building, (800, 0))
            , (AccumulatedDepreciation, (0, 200))
            ]
    assertEqual "edge: nested child precedes and rolls into parent"
        [ (child,
            [ RG.GroupRow (RG.GrossRow AccountsReceivable) (amount False 1000)
            , RG.GroupRow (RG.DeductionRow AllowanceForDoubtfulAccounts) (amount True 100)
            , RG.GroupRow (RG.NetRow RG.TradeReceivablesGroup) (amount False 900)
            ])
        , (parent,
            [ RG.GroupRow (RG.GrossRow Building) (amount False 800)
            , RG.GroupRow (RG.SubgroupRow RG.TradeReceivablesGroup) (amount False 900)
            , RG.GroupRow (RG.DeductionRow AccumulatedDepreciation) (amount True 200)
            , RG.GroupRow (RG.NetRow RG.DepreciableAssetsGroup) (amount False 1500)
            ])
        ]
        (RG.gpBlocks nested)
    assertEqual "edge: only the nested root contributes to totals"
        (M.singleton Assets (1800, 300)) (RG.gpRootTotals nested)
    assertEqual "edge: all nested definition members are consumed"
        (Set.fromList (L.concatMap allMembers [parent, child]))
        (RG.gpConsumed nested)

    let inactiveChild = present [parent, child]
            [ (AccountsReceivable, (1000, 0))
            , (Building, (800, 0))
            , (AccumulatedDepreciation, (0, 200))
            ]
    assertEqual "edge: inactive child gross is not rolled into its parent"
        [(parent,
            [ RG.GroupRow (RG.GrossRow Building) (amount False 800)
            , RG.GroupRow (RG.DeductionRow AccumulatedDepreciation) (amount True 200)
            , RG.GroupRow (RG.NetRow RG.DepreciableAssetsGroup) (amount False 600)
            ])]
        (RG.gpBlocks inactiveChild)
    assertEqual "edge: inactive child gross remains unconsumed"
        (Set.fromList (allMembers parent)) (RG.gpConsumed inactiveChild)
    assertEqual "edge: inactive child cannot inflate the parent total"
        (M.singleton Assets (800, 200)) (RG.gpRootTotals inactiveChild)

    let salesDef = maybe (error "missing NetSalesGroup") id
            (RG.lookupGroupDef defaultDefs RG.NetSalesGroup)
        offsetContra = present [salesDef]
            [ (Sales, (0, 500))
            , (SalesRebates, (50, 50))
            ]
    assertEqual "edge: fully offset contra activity still activates its group"
        [(salesDef,
            [ RG.GroupRow (RG.GrossRow Sales) (amount False 500)
            , RG.GroupRow (RG.NetRow RG.NetSalesGroup) (amount False 500)
            ])]
        (RG.gpBlocks offsetContra)
    assertEqual "edge: fully offset contra title cannot leak to ordinary rows"
        (Set.fromList [Sales, SalesRebates, SalesAllowances]) (RG.gpConsumed offsetContra)

    let rows f b = L.map (L.map T.unpack) (f b)
        excessiveChart = (100 .@ Not:<AccountsReceivable
            .+ 150 .@ Not:<AllowanceForDoubtfulAccounts
            :: EA.Alg Double (HatBase AccountTitles))
        purchasesAndTaxes = (500 .@ Not:<Purchases
            .+ 50 .@ Not:<PurchaseRebates
            .+ 300 .@ Not:<CorporateIncomeTaxes
            .+ 40 .@ Not:<RefundOfIncomeTaxes
            :: EA.Alg Double (HatBase AccountTitles))
        excessiveRows = rows bsRows excessiveChart
        purchasesAndTaxesRows = rows plRows purchasesAndTaxes
    assertEqual "rendering: contra excess signs both net and asset total"
        True
        (["TradeReceivablesNet","-50.0","",""] `elem` excessiveRows
            && ["Total","-50.0","",""] `elem` excessiveRows)
    assertEqual "rendering: purchase rebate and net label are pinned"
        True
        (["PurchaseRebates","-50.0","",""] `elem` purchasesAndTaxesRows
            && ["NetPurchases","450.0","",""] `elem` purchasesAndTaxesRows)
    assertEqual "rendering: tax refund and net label are pinned"
        True
        (["RefundOfIncomeTaxes","-40.0","",""] `elem` purchasesAndTaxesRows
            && ["IncomeTaxesNet","260.0","",""] `elem` purchasesAndTaxesRows)

-- T5/T6: presentation battery。bsRows/plRows と projection の literal 期待値。
-- division projection は contra を含まず, contra は projContraAssets のみが選ぶ
-- (意図的差分: projCurrentLiability/projFixedLiability から当該 2 件が消えた)。
statementFixtureB1, statementFixtureB2, statementFixtureB3, statementFixtureB4, statementFixtureB5 :: BAlg
statementFixtureB1 = 100 .@ Not:<Cash .+ 60 .@ Not:<LoansPayable .+ 40 .@ Not:<CapitalStock
statementFixtureB2 = 500 .@ Not:<Sales .+ 300 .@ Not:<SalesCost
statementFixtureB3 = 1000 .@ Not:<AccountsReceivable .+ 900 .@ Not:<Cash .+ 800 .@ Not:<Building
  .+ 100 .@ Not:<AllowanceForDoubtfulAccounts .+ 200 .@ Not:<AccumulatedDepreciation
  .+ 2000 .@ Not:<CapitalStock .+ 400 .@ Not:<LoansPayable
statementFixtureB4 = 30 .@ Not:<Cash .+ 80 .@ Hat:<Cash
  .+ 100 .@ Not:<AllowanceForDoubtfulAccounts .+ 120 .@ Hat:<AllowanceForDoubtfulAccounts
  .+ 500 .@ Not:<Building .+ 200 .@ Not:<LoansPayable .+ 300 .@ Hat:<LoansPayable
  .+ 250 .@ Not:<AccumulatedDepreciation .+ 50 .@ Hat:<AccumulatedDepreciation
statementFixtureB5 = statementFixtureB3 .+ 300 .@ Not:<SalesCost .+ 500 .@ Not:<Sales .+ 200 .@ Not:<Cash



testStatementRowsAndProjectionsLiteral :: IO ()
testStatementRowsAndProjectionsLiteral = do
    let rows f b = L.map (L.map T.unpack) (f b)
    -- Non-contra statements remain byte-identical. Contra statements use
    -- Land 3 gross -> deduction -> net presentation.
    assertEqual "bsRows b1 (= baseline)"
        [ ["Asset","","Liability",""]
        , ["Cash","100.0","LoansPayable","60.0"]
        , ["Total","100.0","Equity",""]
        , ["","","CapitalStock","40.0"]
        , ["","","Total","100.0"] ]
        (rows bsRows statementFixtureB1)
    assertEqual "bsRows b3 contra groups"
        [ ["Asset","","Liability",""]
        , ["Cash","900.0","LoansPayable","400.0"]
        , ["AccountsReceivable","1000.0","Equity",""]
        , ["AllowanceForDoubtfulAccounts","-100.0","CapitalStock","2000.0"]
        , ["TradeReceivablesNet","900.0","Total","2400.0"]
        , ["Building","800.0","",""]
        , ["AccumulatedDepreciation","-200.0","",""]
        , ["DepreciableAssetsNet","600.0","",""]
        , ["Total","2400.0","",""] ]
        (rows bsRows statementFixtureB3)
    assertEqual "bsRows b4 abnormal balances"
        [ ["Asset","","Liability",""]
        , ["LoansPayable","100.0","Equity",""]
        , ["AllowanceForDoubtfulAccounts","20.0","Total","0.0"]
        , ["TradeReceivablesNet","20.0","",""]
        , ["Building","500.0","",""]
        , ["AccumulatedDepreciation","-200.0","",""]
        , ["DepreciableAssetsNet","300.0","",""]
        , ["Total","420.0","",""] ]
        (rows bsRows statementFixtureB4)
    -- Pre-vocab difference: SalesCost is now closed by the registry-derived
    -- Cost rule, so Sales 500 - SalesCost 300 becomes RetainedEarnings 200.
    assertEqual "bsRows b5 closing and contra groups"
        [ ["Asset","","Liability",""]
        , ["Cash","1100.0","LoansPayable","400.0"]
        , ["AccountsReceivable","1000.0","Equity",""]
        , ["AllowanceForDoubtfulAccounts","-100.0","CapitalStock","2000.0"]
        , ["TradeReceivablesNet","900.0","RetainedEarnings","200.0"]
        , ["Building","800.0","Total","2600.0"]
        , ["AccumulatedDepreciation","-200.0","",""]
        , ["DepreciableAssetsNet","600.0","",""]
        , ["Total","2600.0","",""] ]
        (rows bsRows statementFixtureB5)
    assertEqual "plRows b2 (= baseline)"
        [ ["Cost","","Revenue",""]
        , ["SalesCost","300.0","Sales","500.0"]
        , ["Total","500.0","Total","300.0"] ]
        (rows plRows statementFixtureB2)
    -- projections: 資産系は Land 1 と一致, liability 系は contra が消える (意図的差分),
    -- contra は projContraAssets のみが Hat/Not 双方を保持して選ぶ。
    assertEqual "projCurrentAssets b3 (= baseline)"
        "900.00:@Not:<Cash .+ 1000.00:@Not:<AccountsReceivable"
        (show (EA.projCurrentAssets statementFixtureB3))
    assertEqual "projCurrentLiability b3 (intentional: contra dropped)"
        "400.00:@Not:<LoansPayable"
        (show (EA.projCurrentLiability statementFixtureB3))
    assertEqual "projFixedLiability b3 (intentional: contra dropped)"
        "0"
        (show (EA.projFixedLiability statementFixtureB3))
    assertEqual "projContraAssets b3"
        "100.00:@Not:<AllowanceForDoubtfulAccounts .+ 200.00:@Not:<AccumulatedDepreciation"
        (show (EA.projContraAssets statementFixtureB3))
    assertEqual "projCurrentAssets b4 (= baseline; no contra Hat leakage)"
        "30.00:@Not:<Cash"
        (show (EA.projCurrentAssets statementFixtureB4))
    assertEqual "projFixedAssets b4 (= baseline; no contra Hat leakage)"
        "500.00:@Not:<Building"
        (show (EA.projFixedAssets statementFixtureB4))
    assertEqual "projCurrentLiability b4 (intentional: contra dropped)"
        "200.00:@Not:<LoansPayable"
        (show (EA.projCurrentLiability statementFixtureB4))
    assertEqual "projContraAssets b4 keeps Hat and Not, excludes Cash"
        "120.00:@Hat:<AllowanceForDoubtfulAccounts .+ 100.00:@Not:<AllowanceForDoubtfulAccounts .+ 50.00:@Hat:<AccumulatedDepreciation .+ 250.00:@Not:<AccumulatedDepreciation"
        (show (EA.projContraAssets statementFixtureB4))
    assertEqual "projContraAssets b5"
        "100.00:@Not:<AllowanceForDoubtfulAccounts .+ 200.00:@Not:<AccumulatedDepreciation"
        (show (EA.projContraAssets statementFixtureB5))

-- Current presentation battery. The literal rows are pinned above; this dump
-- covers the remaining row and projection output byte for byte.
statementPresentationLines :: [T.Text]
statementPresentationLines = L.concatMap sect
    [ ("b1-basic", statementFixtureB1), ("b2-pl", statementFixtureB2), ("b3-contra", statementFixtureB3)
    , ("b4-abnormal", statementFixtureB4), ("b5-closing", statementFixtureB5) ]
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

testStatementPresentationGolden :: IO ()
testStatementPresentationGolden =
    goldenCheck "statement presentation golden"
        "test/fixtures/statement-presentation/presentation.txt"
        (T.pack "# statement-presentation battery b1-b5; schema 1\n"
            <> T.unlines statementPresentationLines)

-- HatNot は whichSide で明示 error (規約の regression 固定)
testWhichSideHatNotErrors :: IO ()
testWhichSideHatNotErrors = do
    r <- try (evaluate (whichSide (HatNot :< Cash :: HatBase AccountTitles)))
        :: IO (Either SomeException Side)
    assertEqual "whichSide HatNot policy: explicit error"
        True (case r of Left _ -> True; Right _ -> False)

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
        Just ("profit", amount) -> pure (RM.PeriodProfit amount)
        Just ("loss", amount) -> pure (RM.PeriodLoss amount)
        Just ("break-even", _) -> pure RM.PeriodBreakEven
        Just (direction, _) -> fail
            ("invalid period-result direction for " ++ key ++ ": " ++ direction)
        Nothing -> fail ("missing fixture link: " ++ key)
    fixtureBalance links key = case M.lookup key links of
        Just ("credit", amount) -> pure (TBB.CreditBalance amount)
        Just ("debit", amount) -> pure (TBB.DebitBalance amount)
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
        True (exactBalancedForTest combined)
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
        True (exactBalancedForTest cancellingSet)
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
                RM.PeriodProfit 40 }
        mismatchedInput = fixture { CW._worksheetLinkage = mismatchedLinks }
    assertEqual "consolidation worksheet: P/L to S/S mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet mismatchedInput of
            Left errors -> CW.OwnersPeriodResultLinkMismatch
                (RM.PeriodProfit 50) (RM.PeriodProfit 40) `elem` NE.toList errors
            Right _ -> False)

    let attributionLinks = links
            { CW._profitOrLossNetIncome = RM.PeriodProfit 60 }
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
            { CW._balanceSheetRetainedEarnings = TBB.CreditBalance 139 }
        balanceSheetInput = fixture { CW._worksheetLinkage = balanceSheetLinks }
    assertEqual "consolidation worksheet: S/S to B/S mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet balanceSheetInput of
            Left errors -> CW.BalanceSheetRetainedEarningsMismatch
                (TBB.CreditBalance 140) (TBB.CreditBalance 139)
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
            { CW._balanceSheetNonControllingInterests = TBB.CreditBalance 44 }
        nciBalanceSheetInput = fixture
            { CW._worksheetLinkage = nciBalanceSheetLinks }
    assertEqual "consolidation worksheet: NCI S/S to B/S mismatch is explicit"
        True
        (case CW.validateConsolidationWorksheet nciBalanceSheetInput of
            Left errors -> CW.BalanceSheetNonControllingInterestsMismatch
                (TBB.CreditBalance 45) (TBB.CreditBalance 44)
                `elem` NE.toList errors
            Right _ -> False)

    let lossLinks = links
            { CW._profitOrLossNetIncome = RM.PeriodLoss 25
            , CW._profitOrLossNetIncomeAttributableToOwners = RM.PeriodLoss 20
            , CW._statementOfChangesNetIncomeAttributableToOwners =
                RM.PeriodLoss 20
            , CW._openingRetainedEarnings = TBB.CreditBalance 100
            , CW._retainedEarningsDividends = 10
            , CW._statementOfChangesClosingRetainedEarnings =
                TBB.CreditBalance 70
            , CW._balanceSheetRetainedEarnings = TBB.CreditBalance 70
            , CW._openingNonControllingInterests = TBB.CreditBalance 30
            , CW._nonControllingInterestsPeriodShare = RM.PeriodLoss 5
            , CW._nonControllingInterestsDividends = 5
            , CW._statementOfChangesClosingNonControllingInterests =
                TBB.CreditBalance 20
            , CW._balanceSheetNonControllingInterests = TBB.CreditBalance 20
            }
        lossInput = fixture { CW._worksheetLinkage = lossLinks }
    assertEqual "consolidation worksheet: loss roll-forwards preserve direction"
        True
        (case CW.validateConsolidationWorksheet lossInput of
            Right _ -> True
            Left _  -> False)

    let deficitLinks = links
            { CW._profitOrLossNetIncome = RM.PeriodLoss 50
            , CW._profitOrLossNetIncomeAttributableToOwners = RM.PeriodLoss 50
            , CW._statementOfChangesNetIncomeAttributableToOwners =
                RM.PeriodLoss 50
            , CW._openingRetainedEarnings = TBB.CreditBalance 10
            , CW._retainedEarningsDividends = 0
            , CW._statementOfChangesClosingRetainedEarnings =
                TBB.DebitBalance 40
            , CW._balanceSheetRetainedEarnings = TBB.DebitBalance 40
            , CW._openingNonControllingInterests = TBB.CreditBalance 0
            , CW._nonControllingInterestsPeriodShare = RM.PeriodBreakEven
            , CW._nonControllingInterestsDividends = 0
            , CW._statementOfChangesClosingNonControllingInterests =
                TBB.CreditBalance 0
            , CW._balanceSheetNonControllingInterests = TBB.CreditBalance 0
            }
        deficitInput = fixture { CW._worksheetLinkage = deficitLinks }
    assertEqual "consolidation worksheet: accumulated deficit is structural"
        True
        (case CW.validateConsolidationWorksheet deficitInput of
            Right _ -> True
            Left _  -> False)

    let invalidLinks = links
            { CW._openingRetainedEarnings = TBB.CreditBalance (-1) }
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

testSharedAccountBalancePrimitives :: IO ()
testSharedAccountBalancePrimitives = do
    let balances =
            [ TBB.NoBalance
            , TBB.DebitBalance 7
            , TBB.CreditBalance 11
            ] :: [TBB.AccountBalance Int]
    assertEqual "account balance: pair netting round trip"
        balances (fmap (TBB.netPair . TBB.balancePair) balances)
    assertEqual "account balance: structural sides"
        [Side, Debit, Credit] (fmap TBB.balanceSide balances)

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

testDerivedMetricsAndLegacyCoordinates :: IO ()
testDerivedMetricsAndLegacyCoordinates = do
    assertEqual "legacy derived-coordinate ordinals and Binary bytes"
        [ (NetIncome, 49, T.pack "0031")
        , (GrossProfit, 54, T.pack "0036")
        , (OrdinaryProfit, 55, T.pack "0037")
        , (NetLoss, 64, T.pack "0040")
        , (IncomeSummary, 216, T.pack "00d8")
        ]
        [ (title, fromEnum title, accountSemanticsBinaryHex title)
        | title <- [NetIncome, GrossProfit, OrdinaryProfit, NetLoss, IncomeSummary]
        ]
    assertEqual "exactly four legacy coordinates map to typed metrics"
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
    assertEqual "period metric ignores an inserted legacy balancer"
        (Right (RM.PeriodProfit 100), Right (RM.PeriodProfit 100))
        ( RM.periodResultOfAlg salesOnly
        , RM.periodResultOfAlg afterLegacyBalancer
        )
    assertEqual "Hat is interpreted through account side, not as a scalar sign"
        (Right (RM.PeriodLoss 25)) (RM.periodResultOfAlg hatSales)
    assertEqual "empty nominal basis is break-even"
        (Right RM.PeriodBreakEven :: Either RM.MetricError (RM.PeriodResult MoneyDecimal))
        (RM.periodResultOfAlg (mempty :: CheckedAlgM))
    assertEqual "raw metric boundary rejects wildcard sides"
        (Left (RM.WildcardMetricSide Sales))
        (RM.periodResultOfAlg (10 .@ HatNot :< Sales :: CheckedAlgM))

    let ordinaryLedger = EC.journalFromSides
            [ (Debit, Cash, 100 :: MoneyDecimal)
            , (Credit, Sales, 100)
            ] :: CheckedAlgM
        ordinaryValidated = validateBefore ordinaryLedger
    assertEqual "validated before-closing TB derives one period-result identity"
        (Right (RM.PeriodProfit 100))
        (RM.periodResultOf ordinaryValidated)

    let legacyLedger = EC.journalFromSides
            [ (Debit, NetIncome, 10 :: MoneyDecimal)
            , (Credit, CapitalStock, 10)
            ] :: CheckedAlgM
        legacyValidated = validateBefore legacyLedger
    assertEqual "typed metric rejects a residual legacy coordinate"
        (Left (RM.ResidualDerivedCoordinate NetIncome))
        (RM.periodResultOf legacyValidated)
    assertEqual "legacy intermediate and presentation paths are explicit alternatives"
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
    assertEqual "duplicate metric identity blocks presentation"
        True (case RP.present duplicateContext emptyValidated of
            Left issues -> RP.DuplicateMetricIdentity RM.GrossProfitMetric
                `elem` NE.toList issues
            Right _ -> False)
    let absentAsZeroContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._subtotalDefinitions = [subtotal] }
    assertEqual "canonical subtotal may treat unposted titles as zero"
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
    assertEqual "absent-as-zero does not hide a relabelled non-zero title"
        True (case RP.present removedTitleContext relabelledValidated of
            Left issues -> RP.InvalidSubtotalDefinition RM.GrossProfitMetric
                `elem` NE.toList issues
            Right _ -> False)

    let Just customId = RM.mkMetricId (T.pack "ebitda-adjusted")
        customDefinition = RP.SubtotalDefinition (RM.CustomMetric customId)
            [Sales] [] RP.TreatAbsentAsZero
        unlabelledContext = (RP.jcciSecondGradeContext RP.Standalone)
            { RP._subtotalDefinitions = [customDefinition] }
    assertEqual "custom metric identity requires separate labels"
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
    assertEqual "custom metric identity and profile label are separate"
        [RP.StatementSubtotal (RM.CustomMetric customId)
            (T.pack "Adjusted EBITDA") TB.NoBalance]
        (RP._statementSubtotals (case RP.present labelledContext emptyValidated of
            Right statements -> statements
            Left issues -> error ("labeled custom metric rejected: " ++ show issues)))
    let duplicateLabelContext = labelledContext
            { RP._customMetricLabels = RP._customMetricLabels labelledContext
                ++ RP._customMetricLabels labelledContext
            }
    assertEqual "standalone label lookup rejects duplicate identities"
        Nothing
        (RP.metricLabel duplicateLabelContext (RM.CustomMetric customId)
            TB.NoBalance)
  where
    validateBefore alg = case TB.validateTrialBalance
            TB.strictTrialBalancePolicy (trialBalanceInput alg TB.BeforeClosing) of
        Right value -> value
        Left errors -> error ("Land 5 fixture did not validate: " ++ show errors)

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
    let balancedCases :: [(String, Property)]
        balancedCases =
            [ ("bookkeeping: cogsAdjustmentEntries balanced"
              , forAll genNNDouble $ \beg -> forAll genNNDouble $ \end ->
                    isBalancedD (EB.cogsAdjustmentEntries mkA beg end)
              )
            , ("bookkeeping: depreciationIndirectEntry balanced"
              , forAll genNNDouble $ \amt -> isBalancedD (EB.depreciationIndirectEntry mkA amt)
              )
            , ("bookkeeping: depreciationDirectEntry balanced"
              , forAll genNNDouble $ \amt -> isBalancedD (EB.depreciationDirectEntry mkA amt Fixtures)
              )
            , ("bookkeeping: allowanceReplenishmentEntry balanced"
              , forAll genNNDouble $ \est -> forAll genNNDouble $ \cur ->
                    isBalancedD (EB.allowanceReplenishmentEntry mkA est cur)
              )
            , ("bookkeeping: allowanceResetEntries balanced"
              , forAll genNNDouble $ \est -> forAll genNNDouble $ \cur ->
                    isBalancedD (EB.allowanceResetEntries mkA est cur)
              )
            , ("bookkeeping: prepaidExpenseEntry balanced"
              , forAll genNNDouble $ \amt -> isBalancedD (EB.prepaidExpenseEntry mkA amt RentExpense)
              )
            , ("bookkeeping: unearnedRevenueEntry balanced"
              , forAll genNNDouble $ \amt -> isBalancedD (EB.unearnedRevenueEntry mkA amt RentalIncome)
              )
            , ("bookkeeping: accruedRevenueEntry balanced"
              , forAll genNNDouble $ \amt -> isBalancedD (EB.accruedRevenueEntry mkA amt InterestEarned)
              )
            , ("bookkeeping: accruedExpenseEntry balanced"
              , forAll genNNDouble $ \amt -> isBalancedD (EB.accruedExpenseEntry mkA amt InterestExpense)
              )
            , ("bookkeeping: corporateTaxInterimEntry balanced"
              , forAll genNNDouble $ \amt -> isBalancedD (EB.corporateTaxInterimEntry mkA amt)
              )
            , ("bookkeeping: consumptionTaxSettlementEntry balanced (received>=paid)"
              , forAll genNNDouble $ \paid -> forAll genNNDouble $ \extra ->
                    isBalancedD (EB.consumptionTaxSettlementEntry mkA paid (paid + extra))
              )
            , ("bookkeeping: corporateTaxSettlementEntries balanced (total>=interim)"
              , forAll genNNDouble $ \interim -> forAll genNNDouble $ \extra ->
                    isBalancedD (EB.corporateTaxSettlementEntries mkA (interim + extra) interim)
              )
            , ("bookkeeping: priorPeriodErrorCorrection balanced"
              , forAll genNNDouble $ \curr -> forAll genNNDouble $ \prior ->
                    isBalancedD (EB.priorPeriodErrorCorrection mkA curr prior Depreciation Land)
              )
            ]
    quickProp "bookkeeping: every adjustment builder is balanced" $
        conjoin
            [ counterexample label checkedProperty
            | (label, checkedProperty) <- balancedCases
            ]

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

-- | Run this domain in its original relative test order.
runTests :: IO ()
runTests = do
    testAccountTitlesBinary
    testAllowanceAccountTitles
    testAccountTitleClassification
    testFinalStockRuleReference
    testVocabOrdinalPin
    testIncomeSummaryBalancedNoCrash
    testAssistAllAccountInfos
    testAccountSemanticsRegistryInvariants
    testAccountSemanticsGolden
    testAssistSuggestAccounts
    testPostVocabGolden
    testAccountAlgebraBehaviorGolden
    testWriteRowsGolden
    testReadoutBaselineGolden
    testExampleNumbersGolden
    testAdmissionBaselineGolden
    testAliasResolutionGolden
    testJcciAccountNameCoverage
    testJapaneseAccountLabels
    testRegistryWildcards
    testContraIffReversedHomeSide
    testPimoFlipInvolution
    testExchangeRelationProp538
    testIsContraSweepAcrossBaseInstances
    testPresentationGroups
    testStatementRowsAndProjectionsLiteral
    testStatementPresentationGolden
    testWhichSideHatNotErrors
    testConsolidationWorksheet
    testSharedAccountBalancePrimitives
    testTrialBalanceValidation
    testReportingPresentation
    testDerivedMetricsAndLegacyCoordinates
    bookkeepingProperties
    closingDocsTests
