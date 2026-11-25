{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- | Account titles for ledgers limited to the library's JCCI vocabulary.
--
-- Import this module for 'JcciTitles'. Import @ExchangeAlgebra.Accounting.Account@
-- for the chart that also includes macroeconomic accounts. Use qualified imports
-- when working with both charts in one module.
--
-- When adding a JCCI title, add its constructor and row to the account title
-- table. Add the matching pattern to this module's export list, type signature,
-- definitions, and @COMPLETE@ pragma in constructor order.
module ExchangeAlgebra.Accounting.Account.Jcci
    ( JcciTitles
    , widen
    , narrow
    , narrowAlg
    , pattern Cash
    , pattern Deposits
    , pattern CurrentDeposits
    , pattern Securities
    , pattern InvestmentSecurities
    , pattern InvestmentInAssociate
    , pattern LongTermNationalBonds
    , pattern ShortTermNationalBonds
    , pattern Products
    , pattern Machinery
    , pattern Building
    , pattern Vehicle
    , pattern StockInvestment
    , pattern EquipmentInvestment
    , pattern LongTermLoansReceivable
    , pattern AccountsReceivable
    , pattern ShortTermLoansReceivable
    , pattern Gold
    , pattern CapitalStock
    , pattern RetainedEarnings
    , pattern LongTermLoansPayable
    , pattern ShortTermLoansPayable
    , pattern LoansPayable
    , pattern LongTermNationalBondsPayable
    , pattern ShortTermNationalBondsPayable
    , pattern Depreciation
    , pattern AmortizationExpense
    , pattern SalesCost
    , pattern BusinessTrip
    , pattern UtilitiesExpense
    , pattern RentExpense
    , pattern AdvertisingExpense
    , pattern DeliveryExpenses
    , pattern SuppliesExpenses
    , pattern MiscellaneousExpenses
    , pattern WageExpenditure
    , pattern InterestExpense
    , pattern TaxesExpense
    , pattern Purchases
    , pattern NetIncome
    , pattern NationalBondInterestEarned
    , pattern DepositInterestEarned
    , pattern GrossProfit
    , pattern OrdinaryProfit
    , pattern InterestEarned
    , pattern ReceiptFee
    , pattern RentalIncome
    , pattern Sales
    , pattern EquityInEarningsOfInvestee
    , pattern NetLoss
    , pattern PettyCash
    , pattern NotesReceivable
    , pattern ElectronicallyRecordedReceivable
    , pattern CreditCardReceivable
    , pattern NotesLoansReceivable
    , pattern MerchandiseInventory
    , pattern AdvancesPaid
    , pattern PrepaidExpenses
    , pattern AccruedRevenue
    , pattern OtherReceivables
    , pattern PaymentsOnBehalf
    , pattern SuspensePayments
    , pattern ConsumptionTaxPaid
    , pattern PrepaidCorporateIncomeTaxes
    , pattern Land
    , pattern Fixtures
    , pattern Patent
    , pattern Trademark
    , pattern Software
    , pattern CashOverShort
    , pattern AccountsPayable
    , pattern NotesPayable
    , pattern ElectronicallyRecordedObligations
    , pattern NotesLoansPayable
    , pattern BankOverdraft
    , pattern AdvancesReceived
    , pattern UnearnedRevenue
    , pattern AccruedExpenses
    , pattern OtherPayables
    , pattern DepositsReceived
    , pattern SuspenseReceipts
    , pattern ConsumptionTaxReceived
    , pattern AccruedConsumptionTax
    , pattern AccruedCorporateIncomeTaxes
    , pattern UnpaidDividends
    , pattern AllowanceForDoubtfulAccounts
    , pattern AccumulatedDepreciation
    , pattern LegalRetainedEarnings
    , pattern CumulativeTranslationAdjustment
    , pattern ProvisionForDoubtfulAccounts
    , pattern BadDebtLoss
    , pattern LossOnSalesOfFixedAssets
    , pattern LossOnSalesOfNotesReceivable
    , pattern PaymentFees
    , pattern MiscellaneousLoss
    , pattern CorporateIncomeTaxes
    , pattern CommunicationExpenses
    , pattern GainOnSalesOfFixedAssets
    , pattern RecoveryOfBadDebts
    , pattern MiscellaneousIncome
    , pattern ReversalOfAllowanceForDoubtfulAccounts
    , pattern TimeDeposits
    , pattern LoansReceivable
    , pattern GiftCertificatesReceived
    , pattern SecurityDepositsPaid
    , pattern SuppliesOnHand
    , pattern ContractAssets
    , pattern IncomeTaxesRefundReceivable
    , pattern WorkInProcess
    , pattern DeferredTaxAssets
    , pattern LeasedAssets
    , pattern ToolsAndInstruments
    , pattern ConstructionInProgress
    , pattern Goodwill
    , pattern SoftwareInProgress
    , pattern LongTermPrepaidExpenses
    , pattern DishonoredNotesReceivable
    , pattern PrepaidPensionCost
    , pattern NetDefinedBenefitAsset
    , pattern DepositsInSpecialAccounts
    , pattern Structures
    , pattern LeaseholdRights
    , pattern NonOperatingNotesReceivable
    , pattern NonOperatingElectronicallyRecordedReceivable
    , pattern RefundLiabilities
    , pattern NonOperatingNotesPayable
    , pattern NonOperatingElectronicallyRecordedObligations
    , pattern BonusesPayable
    , pattern AllowanceForRepairs
    , pattern AllowanceForProductWarranties
    , pattern AllowanceForBonuses
    , pattern DeferredTaxLiabilities
    , pattern LeaseObligations
    , pattern GuaranteeDepositsReceived
    , pattern AllowanceForRetirementBenefits
    , pattern LongTermOtherPayables
    , pattern NetDefinedBenefitLiability
    , pattern StockSubscriptionDeposits
    , pattern LegalCapitalSurplus
    , pattern OtherCapitalSurplus
    , pattern DividendEqualizationReserve
    , pattern RepairFundReserve
    , pattern ConstructionFundReserve
    , pattern GeneralReserve
    , pattern ValuationDifferenceOnOtherSecurities
    , pattern NonControllingInterests
    , pattern CapitalSurplus
    , pattern EarnedSurplus
    , pattern ServiceRevenue
    , pattern OperatingRevenue
    , pattern GainOnSalesOfSecurities
    , pattern GainOnValuationOfSecurities
    , pattern DividendsReceived
    , pattern InterestOnSecurities
    , pattern GainOnSalesOfInvestmentSecurities
    , pattern InsuranceGain
    , pattern GainOnBargainPurchase
    , pattern ReversalOfAllowanceForRepairs
    , pattern ReversalOfAllowanceForProductWarranties
    , pattern GainOnDonationOfFixedAssets
    , pattern GainOnNationalSubsidies
    , pattern GainOnConstructionGrants
    , pattern LandRentReceived
    , pattern SalesRebates
    , pattern CostOfServices
    , pattern OperatingExpenses
    , pattern InventoryShrinkageLoss
    , pattern LossOnValuationOfMerchandise
    , pattern Bonuses
    , pattern RetirementBenefitExpenses
    , pattern ProvisionForRepairs
    , pattern ProvisionForBonuses
    , pattern ProvisionForProductWarranties
    , pattern ResearchAndDevelopmentExpenses
    , pattern AmortizationOfGoodwill
    , pattern AmortizationOfSoftware
    , pattern AmortizationOfPatents
    , pattern LeaseExpenses
    , pattern IncorporationExpenses
    , pattern StockIssuanceCosts
    , pattern BusinessCommencementExpenses
    , pattern DevelopmentExpenses
    , pattern LossOnSalesOfElectronicallyRecordedReceivables
    , pattern LossOnSalesOfReceivables
    , pattern LossOnSalesOfSecurities
    , pattern LossOnValuationOfSecurities
    , pattern LossOnSalesOfInvestmentSecurities
    , pattern LossOnFire
    , pattern LossOnRetirementOfFixedAssets
    , pattern LossOnReductionOfFixedAssets
    , pattern AdditionalIncomeTaxesForPriorPeriods
    , pattern RefundOfIncomeTaxes
    , pattern PurchaseRebates
    , pattern WelfareExpenses
    , pattern MaintenanceExpenses
    , pattern StatutoryWelfareExpenses
    , pattern LandRentPaid
    , pattern InsuranceExpense
    , pattern RepairsExpense
    , pattern StorageExpenses
    , pattern MembershipFees
    , pattern IncomeSummary
    , pattern SuspenseAccount
    , pattern ForeignExchangeGains
    , pattern ForeignExchangeLosses
    , pattern ContraAccountForGuaranteeObligations
    , pattern GuaranteeObligations
    , pattern IncomeTaxesAdjustment
    , pattern BranchCurrentAccount
    , pattern HeadOfficeCurrentAccount
    , pattern NetIncomeAttributableToNCI
    , pattern NetLossAttributableToNCI
    , pattern TradingSecurities
    , pattern HeldToMaturityBonds
    , pattern SubsidiaryStocks
    , pattern AffiliateStocks
    , pattern AvailableForSaleSecurities
    , pattern ConsumptionTaxRefundReceivable
    , pattern PropertyTaxPayable
    , pattern DepositsReceivedFromOfficers
    , pattern EntertainmentExpenses
    , pattern MeetingExpenses
    , pattern NewspaperBooksExpenses
    , pattern RawMaterials
    , pattern GoodsInTransit
    , pattern SalesAllowances
    , pattern PurchaseAllowances
    , pattern ReversalOfAllowanceForBonuses
    , pattern DirectorsCompensations
    , pattern AnyJcci
    ) where

import Control.DeepSeq (NFData)
import qualified Data.Binary as Binary
import Data.Hashable (Hashable)
import qualified Data.Set as Set
import ExchangeAlgebra.Accounting.Account.Classification (Side(..))
import ExchangeAlgebra.Accounting.Account.Title (AccountTitles)
import qualified ExchangeAlgebra.Accounting.Account.Title as T
import ExchangeAlgebra.Accounting.Account.Vocabulary (AccountVocabulary(..))
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Algebra.Base (BaseClass, Element(..), HatBase(..))
import ExchangeAlgebra.Algebra.Core (Alg, (.@), foldEntries, unionsMerge)
import ExchangeAlgebra.Algebra.Value (HatVal)

-- | A JCCI account title or the wildcard 'AnyJcci'.
newtype JcciTitles = JcciTitles AccountTitles
    deriving newtype (Eq, Ord, Show, NFData, Hashable)

instance Binary.Binary JcciTitles where
    put = Binary.put . widen
    get = do
        title <- Binary.get
        case narrow title of
            Just jcci -> pure jcci
            Nothing -> fail ("JcciTitles: macroeconomic account " ++ show title)

instance Element JcciTitles where
    wildcard = AnyJcci

instance BaseClass JcciTitles

instance AccountVocabulary JcciTitles where
    vocabSpec = vocabSpec . widen
    vocabSemantics = vocabSemantics . widen
    vocabImpossibleBalanceSides =
        [ (Cash, Credit)
        , (PettyCash, Credit)
        , (CapitalStock, Debit)
        , (AccumulatedDepreciation, Debit)
        ]
    vocabSuspenseTitles = [SuspensePayments, SuspenseReceipts, SuspenseAccount]
    vocabAdditionalClosingResidualTitles = [CashOverShort]
    vocabReciprocalPairs = [(BranchCurrentAccount, HeadOfficeCurrentAccount)]
    closingTarget = RetainedEarnings
    vocabDivision = vocabDivision . widen
    vocabContra = vocabContra . widen

instance ExBaseClass (HatBase JcciTitles) where
    type TitleOf (HatBase JcciTitles) = JcciTitles
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | Embed a JCCI title in the full chart, mapping 'AnyJcci' to the full chart's wildcard.
--
-- Use @mapBasePart widen ledger@ to embed a ledger. The relabeling preserves
-- term values, Hat and Not directions, and the order of terms with the same base.
widen :: JcciTitles -> AccountTitles
widen (JcciTitles title) = title

-- | Restrict a full-chart title to the JCCI vocabulary.
-- The full chart's wildcard maps to 'AnyJcci'. Macroeconomic titles return 'Nothing'.
narrow :: AccountTitles -> Maybe JcciTitles
narrow title
    | T.isJcciTitle title = Just (JcciTitles title)
    | otherwise = Nothing

-- | Restrict a ledger with account-only bases to the JCCI vocabulary.
-- On success, preserve term values, Hat and Not directions, and each base's term order.
-- If any term uses a macroeconomic title, return all such titles once in ascending order.
narrowAlg
    :: HatVal v
    => Alg v (HatBase AccountTitles)
    -> Either [AccountTitles] (Alg v (HatBase JcciTitles))
narrowAlg ledger = case Set.toAscList rejected of
    [] -> Right (unionsMerge (reverse terms))
    titles -> Left titles
  where
    -- Collect with cons, then pass the terms to unionsMerge in traversal order.
    (rejected, terms) = foldEntries collect (Set.empty, []) ledger

    collect (!missing, accepted) value (side :< title) =
        case narrow title of
            Nothing -> (Set.insert title missing, accepted)
            Just jcci -> (missing, (value .@ side :< jcci) : accepted)

-- | Account title patterns in the full chart's declaration order.
pattern Cash
    , Deposits
    , CurrentDeposits
    , Securities
    , InvestmentSecurities
    , InvestmentInAssociate
    , LongTermNationalBonds
    , ShortTermNationalBonds
    , Products
    , Machinery
    , Building
    , Vehicle
    , StockInvestment
    , EquipmentInvestment
    , LongTermLoansReceivable
    , AccountsReceivable
    , ShortTermLoansReceivable
    , Gold
    , CapitalStock
    , RetainedEarnings
    , LongTermLoansPayable
    , ShortTermLoansPayable
    , LoansPayable
    , LongTermNationalBondsPayable
    , ShortTermNationalBondsPayable
    , Depreciation
    , AmortizationExpense
    , SalesCost
    , BusinessTrip
    , UtilitiesExpense
    , RentExpense
    , AdvertisingExpense
    , DeliveryExpenses
    , SuppliesExpenses
    , MiscellaneousExpenses
    , WageExpenditure
    , InterestExpense
    , TaxesExpense
    , Purchases
    , NetIncome
    , NationalBondInterestEarned
    , DepositInterestEarned
    , GrossProfit
    , OrdinaryProfit
    , InterestEarned
    , ReceiptFee
    , RentalIncome
    , Sales
    , EquityInEarningsOfInvestee
    , NetLoss
    , PettyCash
    , NotesReceivable
    , ElectronicallyRecordedReceivable
    , CreditCardReceivable
    , NotesLoansReceivable
    , MerchandiseInventory
    , AdvancesPaid
    , PrepaidExpenses
    , AccruedRevenue
    , OtherReceivables
    , PaymentsOnBehalf
    , SuspensePayments
    , ConsumptionTaxPaid
    , PrepaidCorporateIncomeTaxes
    , Land
    , Fixtures
    , Patent
    , Trademark
    , Software
    , CashOverShort
    , AccountsPayable
    , NotesPayable
    , ElectronicallyRecordedObligations
    , NotesLoansPayable
    , BankOverdraft
    , AdvancesReceived
    , UnearnedRevenue
    , AccruedExpenses
    , OtherPayables
    , DepositsReceived
    , SuspenseReceipts
    , ConsumptionTaxReceived
    , AccruedConsumptionTax
    , AccruedCorporateIncomeTaxes
    , UnpaidDividends
    , AllowanceForDoubtfulAccounts
    , AccumulatedDepreciation
    , LegalRetainedEarnings
    , CumulativeTranslationAdjustment
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
    , TimeDeposits
    , LoansReceivable
    , GiftCertificatesReceived
    , SecurityDepositsPaid
    , SuppliesOnHand
    , ContractAssets
    , IncomeTaxesRefundReceivable
    , WorkInProcess
    , DeferredTaxAssets
    , LeasedAssets
    , ToolsAndInstruments
    , ConstructionInProgress
    , Goodwill
    , SoftwareInProgress
    , LongTermPrepaidExpenses
    , DishonoredNotesReceivable
    , PrepaidPensionCost
    , NetDefinedBenefitAsset
    , DepositsInSpecialAccounts
    , Structures
    , LeaseholdRights
    , NonOperatingNotesReceivable
    , NonOperatingElectronicallyRecordedReceivable
    , RefundLiabilities
    , NonOperatingNotesPayable
    , NonOperatingElectronicallyRecordedObligations
    , BonusesPayable
    , AllowanceForRepairs
    , AllowanceForProductWarranties
    , AllowanceForBonuses
    , DeferredTaxLiabilities
    , LeaseObligations
    , GuaranteeDepositsReceived
    , AllowanceForRetirementBenefits
    , LongTermOtherPayables
    , NetDefinedBenefitLiability
    , StockSubscriptionDeposits
    , LegalCapitalSurplus
    , OtherCapitalSurplus
    , DividendEqualizationReserve
    , RepairFundReserve
    , ConstructionFundReserve
    , GeneralReserve
    , ValuationDifferenceOnOtherSecurities
    , NonControllingInterests
    , CapitalSurplus
    , EarnedSurplus
    , ServiceRevenue
    , OperatingRevenue
    , GainOnSalesOfSecurities
    , GainOnValuationOfSecurities
    , DividendsReceived
    , InterestOnSecurities
    , GainOnSalesOfInvestmentSecurities
    , InsuranceGain
    , GainOnBargainPurchase
    , ReversalOfAllowanceForRepairs
    , ReversalOfAllowanceForProductWarranties
    , GainOnDonationOfFixedAssets
    , GainOnNationalSubsidies
    , GainOnConstructionGrants
    , LandRentReceived
    , SalesRebates
    , CostOfServices
    , OperatingExpenses
    , InventoryShrinkageLoss
    , LossOnValuationOfMerchandise
    , Bonuses
    , RetirementBenefitExpenses
    , ProvisionForRepairs
    , ProvisionForBonuses
    , ProvisionForProductWarranties
    , ResearchAndDevelopmentExpenses
    , AmortizationOfGoodwill
    , AmortizationOfSoftware
    , AmortizationOfPatents
    , LeaseExpenses
    , IncorporationExpenses
    , StockIssuanceCosts
    , BusinessCommencementExpenses
    , DevelopmentExpenses
    , LossOnSalesOfElectronicallyRecordedReceivables
    , LossOnSalesOfReceivables
    , LossOnSalesOfSecurities
    , LossOnValuationOfSecurities
    , LossOnSalesOfInvestmentSecurities
    , LossOnFire
    , LossOnRetirementOfFixedAssets
    , LossOnReductionOfFixedAssets
    , AdditionalIncomeTaxesForPriorPeriods
    , RefundOfIncomeTaxes
    , PurchaseRebates
    , WelfareExpenses
    , MaintenanceExpenses
    , StatutoryWelfareExpenses
    , LandRentPaid
    , InsuranceExpense
    , RepairsExpense
    , StorageExpenses
    , MembershipFees
    , IncomeSummary
    , SuspenseAccount
    , ForeignExchangeGains
    , ForeignExchangeLosses
    , ContraAccountForGuaranteeObligations
    , GuaranteeObligations
    , IncomeTaxesAdjustment
    , BranchCurrentAccount
    , HeadOfficeCurrentAccount
    , NetIncomeAttributableToNCI
    , NetLossAttributableToNCI
    , TradingSecurities
    , HeldToMaturityBonds
    , SubsidiaryStocks
    , AffiliateStocks
    , AvailableForSaleSecurities
    , ConsumptionTaxRefundReceivable
    , PropertyTaxPayable
    , DepositsReceivedFromOfficers
    , EntertainmentExpenses
    , MeetingExpenses
    , NewspaperBooksExpenses
    , RawMaterials
    , GoodsInTransit
    , SalesAllowances
    , PurchaseAllowances
    , ReversalOfAllowanceForBonuses
    , DirectorsCompensations
    :: JcciTitles
pattern Cash = JcciTitles T.Cash
pattern Deposits = JcciTitles T.Deposits
pattern CurrentDeposits = JcciTitles T.CurrentDeposits
pattern Securities = JcciTitles T.Securities
pattern InvestmentSecurities = JcciTitles T.InvestmentSecurities
pattern InvestmentInAssociate = JcciTitles T.InvestmentInAssociate
pattern LongTermNationalBonds = JcciTitles T.LongTermNationalBonds
pattern ShortTermNationalBonds = JcciTitles T.ShortTermNationalBonds
pattern Products = JcciTitles T.Products
pattern Machinery = JcciTitles T.Machinery
pattern Building = JcciTitles T.Building
pattern Vehicle = JcciTitles T.Vehicle
pattern StockInvestment = JcciTitles T.StockInvestment
pattern EquipmentInvestment = JcciTitles T.EquipmentInvestment
pattern LongTermLoansReceivable = JcciTitles T.LongTermLoansReceivable
pattern AccountsReceivable = JcciTitles T.AccountsReceivable
pattern ShortTermLoansReceivable = JcciTitles T.ShortTermLoansReceivable
pattern Gold = JcciTitles T.Gold
pattern CapitalStock = JcciTitles T.CapitalStock
pattern RetainedEarnings = JcciTitles T.RetainedEarnings
pattern LongTermLoansPayable = JcciTitles T.LongTermLoansPayable
pattern ShortTermLoansPayable = JcciTitles T.ShortTermLoansPayable
pattern LoansPayable = JcciTitles T.LoansPayable
pattern LongTermNationalBondsPayable = JcciTitles T.LongTermNationalBondsPayable
pattern ShortTermNationalBondsPayable = JcciTitles T.ShortTermNationalBondsPayable
pattern Depreciation = JcciTitles T.Depreciation
pattern AmortizationExpense = JcciTitles T.AmortizationExpense
pattern SalesCost = JcciTitles T.SalesCost
pattern BusinessTrip = JcciTitles T.BusinessTrip
pattern UtilitiesExpense = JcciTitles T.UtilitiesExpense
pattern RentExpense = JcciTitles T.RentExpense
pattern AdvertisingExpense = JcciTitles T.AdvertisingExpense
pattern DeliveryExpenses = JcciTitles T.DeliveryExpenses
pattern SuppliesExpenses = JcciTitles T.SuppliesExpenses
pattern MiscellaneousExpenses = JcciTitles T.MiscellaneousExpenses
pattern WageExpenditure = JcciTitles T.WageExpenditure
pattern InterestExpense = JcciTitles T.InterestExpense
pattern TaxesExpense = JcciTitles T.TaxesExpense
pattern Purchases = JcciTitles T.Purchases
pattern NetIncome = JcciTitles T.NetIncome
pattern NationalBondInterestEarned = JcciTitles T.NationalBondInterestEarned
pattern DepositInterestEarned = JcciTitles T.DepositInterestEarned
pattern GrossProfit = JcciTitles T.GrossProfit
pattern OrdinaryProfit = JcciTitles T.OrdinaryProfit
pattern InterestEarned = JcciTitles T.InterestEarned
pattern ReceiptFee = JcciTitles T.ReceiptFee
pattern RentalIncome = JcciTitles T.RentalIncome
pattern Sales = JcciTitles T.Sales
pattern EquityInEarningsOfInvestee = JcciTitles T.EquityInEarningsOfInvestee
pattern NetLoss = JcciTitles T.NetLoss
pattern PettyCash = JcciTitles T.PettyCash
pattern NotesReceivable = JcciTitles T.NotesReceivable
pattern ElectronicallyRecordedReceivable = JcciTitles T.ElectronicallyRecordedReceivable
pattern CreditCardReceivable = JcciTitles T.CreditCardReceivable
pattern NotesLoansReceivable = JcciTitles T.NotesLoansReceivable
pattern MerchandiseInventory = JcciTitles T.MerchandiseInventory
pattern AdvancesPaid = JcciTitles T.AdvancesPaid
pattern PrepaidExpenses = JcciTitles T.PrepaidExpenses
pattern AccruedRevenue = JcciTitles T.AccruedRevenue
pattern OtherReceivables = JcciTitles T.OtherReceivables
pattern PaymentsOnBehalf = JcciTitles T.PaymentsOnBehalf
pattern SuspensePayments = JcciTitles T.SuspensePayments
pattern ConsumptionTaxPaid = JcciTitles T.ConsumptionTaxPaid
pattern PrepaidCorporateIncomeTaxes = JcciTitles T.PrepaidCorporateIncomeTaxes
pattern Land = JcciTitles T.Land
pattern Fixtures = JcciTitles T.Fixtures
pattern Patent = JcciTitles T.Patent
pattern Trademark = JcciTitles T.Trademark
pattern Software = JcciTitles T.Software
pattern CashOverShort = JcciTitles T.CashOverShort
pattern AccountsPayable = JcciTitles T.AccountsPayable
pattern NotesPayable = JcciTitles T.NotesPayable
pattern ElectronicallyRecordedObligations = JcciTitles T.ElectronicallyRecordedObligations
pattern NotesLoansPayable = JcciTitles T.NotesLoansPayable
pattern BankOverdraft = JcciTitles T.BankOverdraft
pattern AdvancesReceived = JcciTitles T.AdvancesReceived
pattern UnearnedRevenue = JcciTitles T.UnearnedRevenue
pattern AccruedExpenses = JcciTitles T.AccruedExpenses
pattern OtherPayables = JcciTitles T.OtherPayables
pattern DepositsReceived = JcciTitles T.DepositsReceived
pattern SuspenseReceipts = JcciTitles T.SuspenseReceipts
pattern ConsumptionTaxReceived = JcciTitles T.ConsumptionTaxReceived
pattern AccruedConsumptionTax = JcciTitles T.AccruedConsumptionTax
pattern AccruedCorporateIncomeTaxes = JcciTitles T.AccruedCorporateIncomeTaxes
pattern UnpaidDividends = JcciTitles T.UnpaidDividends
pattern AllowanceForDoubtfulAccounts = JcciTitles T.AllowanceForDoubtfulAccounts
pattern AccumulatedDepreciation = JcciTitles T.AccumulatedDepreciation
pattern LegalRetainedEarnings = JcciTitles T.LegalRetainedEarnings
pattern CumulativeTranslationAdjustment = JcciTitles T.CumulativeTranslationAdjustment
pattern ProvisionForDoubtfulAccounts = JcciTitles T.ProvisionForDoubtfulAccounts
pattern BadDebtLoss = JcciTitles T.BadDebtLoss
pattern LossOnSalesOfFixedAssets = JcciTitles T.LossOnSalesOfFixedAssets
pattern LossOnSalesOfNotesReceivable = JcciTitles T.LossOnSalesOfNotesReceivable
pattern PaymentFees = JcciTitles T.PaymentFees
pattern MiscellaneousLoss = JcciTitles T.MiscellaneousLoss
pattern CorporateIncomeTaxes = JcciTitles T.CorporateIncomeTaxes
pattern CommunicationExpenses = JcciTitles T.CommunicationExpenses
pattern GainOnSalesOfFixedAssets = JcciTitles T.GainOnSalesOfFixedAssets
pattern RecoveryOfBadDebts = JcciTitles T.RecoveryOfBadDebts
pattern MiscellaneousIncome = JcciTitles T.MiscellaneousIncome
pattern ReversalOfAllowanceForDoubtfulAccounts = JcciTitles T.ReversalOfAllowanceForDoubtfulAccounts
pattern TimeDeposits = JcciTitles T.TimeDeposits
pattern LoansReceivable = JcciTitles T.LoansReceivable
pattern GiftCertificatesReceived = JcciTitles T.GiftCertificatesReceived
pattern SecurityDepositsPaid = JcciTitles T.SecurityDepositsPaid
pattern SuppliesOnHand = JcciTitles T.SuppliesOnHand
pattern ContractAssets = JcciTitles T.ContractAssets
pattern IncomeTaxesRefundReceivable = JcciTitles T.IncomeTaxesRefundReceivable
pattern WorkInProcess = JcciTitles T.WorkInProcess
pattern DeferredTaxAssets = JcciTitles T.DeferredTaxAssets
pattern LeasedAssets = JcciTitles T.LeasedAssets
pattern ToolsAndInstruments = JcciTitles T.ToolsAndInstruments
pattern ConstructionInProgress = JcciTitles T.ConstructionInProgress
pattern Goodwill = JcciTitles T.Goodwill
pattern SoftwareInProgress = JcciTitles T.SoftwareInProgress
pattern LongTermPrepaidExpenses = JcciTitles T.LongTermPrepaidExpenses
pattern DishonoredNotesReceivable = JcciTitles T.DishonoredNotesReceivable
pattern PrepaidPensionCost = JcciTitles T.PrepaidPensionCost
pattern NetDefinedBenefitAsset = JcciTitles T.NetDefinedBenefitAsset
pattern DepositsInSpecialAccounts = JcciTitles T.DepositsInSpecialAccounts
pattern Structures = JcciTitles T.Structures
pattern LeaseholdRights = JcciTitles T.LeaseholdRights
pattern NonOperatingNotesReceivable = JcciTitles T.NonOperatingNotesReceivable
pattern NonOperatingElectronicallyRecordedReceivable =
    JcciTitles T.NonOperatingElectronicallyRecordedReceivable
pattern RefundLiabilities = JcciTitles T.RefundLiabilities
pattern NonOperatingNotesPayable = JcciTitles T.NonOperatingNotesPayable
pattern NonOperatingElectronicallyRecordedObligations =
    JcciTitles T.NonOperatingElectronicallyRecordedObligations
pattern BonusesPayable = JcciTitles T.BonusesPayable
pattern AllowanceForRepairs = JcciTitles T.AllowanceForRepairs
pattern AllowanceForProductWarranties = JcciTitles T.AllowanceForProductWarranties
pattern AllowanceForBonuses = JcciTitles T.AllowanceForBonuses
pattern DeferredTaxLiabilities = JcciTitles T.DeferredTaxLiabilities
pattern LeaseObligations = JcciTitles T.LeaseObligations
pattern GuaranteeDepositsReceived = JcciTitles T.GuaranteeDepositsReceived
pattern AllowanceForRetirementBenefits = JcciTitles T.AllowanceForRetirementBenefits
pattern LongTermOtherPayables = JcciTitles T.LongTermOtherPayables
pattern NetDefinedBenefitLiability = JcciTitles T.NetDefinedBenefitLiability
pattern StockSubscriptionDeposits = JcciTitles T.StockSubscriptionDeposits
pattern LegalCapitalSurplus = JcciTitles T.LegalCapitalSurplus
pattern OtherCapitalSurplus = JcciTitles T.OtherCapitalSurplus
pattern DividendEqualizationReserve = JcciTitles T.DividendEqualizationReserve
pattern RepairFundReserve = JcciTitles T.RepairFundReserve
pattern ConstructionFundReserve = JcciTitles T.ConstructionFundReserve
pattern GeneralReserve = JcciTitles T.GeneralReserve
pattern ValuationDifferenceOnOtherSecurities = JcciTitles T.ValuationDifferenceOnOtherSecurities
pattern NonControllingInterests = JcciTitles T.NonControllingInterests
pattern CapitalSurplus = JcciTitles T.CapitalSurplus
pattern EarnedSurplus = JcciTitles T.EarnedSurplus
pattern ServiceRevenue = JcciTitles T.ServiceRevenue
pattern OperatingRevenue = JcciTitles T.OperatingRevenue
pattern GainOnSalesOfSecurities = JcciTitles T.GainOnSalesOfSecurities
pattern GainOnValuationOfSecurities = JcciTitles T.GainOnValuationOfSecurities
pattern DividendsReceived = JcciTitles T.DividendsReceived
pattern InterestOnSecurities = JcciTitles T.InterestOnSecurities
pattern GainOnSalesOfInvestmentSecurities = JcciTitles T.GainOnSalesOfInvestmentSecurities
pattern InsuranceGain = JcciTitles T.InsuranceGain
pattern GainOnBargainPurchase = JcciTitles T.GainOnBargainPurchase
pattern ReversalOfAllowanceForRepairs = JcciTitles T.ReversalOfAllowanceForRepairs
pattern ReversalOfAllowanceForProductWarranties =
    JcciTitles T.ReversalOfAllowanceForProductWarranties
pattern GainOnDonationOfFixedAssets = JcciTitles T.GainOnDonationOfFixedAssets
pattern GainOnNationalSubsidies = JcciTitles T.GainOnNationalSubsidies
pattern GainOnConstructionGrants = JcciTitles T.GainOnConstructionGrants
pattern LandRentReceived = JcciTitles T.LandRentReceived
pattern SalesRebates = JcciTitles T.SalesRebates
pattern CostOfServices = JcciTitles T.CostOfServices
pattern OperatingExpenses = JcciTitles T.OperatingExpenses
pattern InventoryShrinkageLoss = JcciTitles T.InventoryShrinkageLoss
pattern LossOnValuationOfMerchandise = JcciTitles T.LossOnValuationOfMerchandise
pattern Bonuses = JcciTitles T.Bonuses
pattern RetirementBenefitExpenses = JcciTitles T.RetirementBenefitExpenses
pattern ProvisionForRepairs = JcciTitles T.ProvisionForRepairs
pattern ProvisionForBonuses = JcciTitles T.ProvisionForBonuses
pattern ProvisionForProductWarranties = JcciTitles T.ProvisionForProductWarranties
pattern ResearchAndDevelopmentExpenses = JcciTitles T.ResearchAndDevelopmentExpenses
pattern AmortizationOfGoodwill = JcciTitles T.AmortizationOfGoodwill
pattern AmortizationOfSoftware = JcciTitles T.AmortizationOfSoftware
pattern AmortizationOfPatents = JcciTitles T.AmortizationOfPatents
pattern LeaseExpenses = JcciTitles T.LeaseExpenses
pattern IncorporationExpenses = JcciTitles T.IncorporationExpenses
pattern StockIssuanceCosts = JcciTitles T.StockIssuanceCosts
pattern BusinessCommencementExpenses = JcciTitles T.BusinessCommencementExpenses
pattern DevelopmentExpenses = JcciTitles T.DevelopmentExpenses
pattern LossOnSalesOfElectronicallyRecordedReceivables =
    JcciTitles T.LossOnSalesOfElectronicallyRecordedReceivables
pattern LossOnSalesOfReceivables = JcciTitles T.LossOnSalesOfReceivables
pattern LossOnSalesOfSecurities = JcciTitles T.LossOnSalesOfSecurities
pattern LossOnValuationOfSecurities = JcciTitles T.LossOnValuationOfSecurities
pattern LossOnSalesOfInvestmentSecurities = JcciTitles T.LossOnSalesOfInvestmentSecurities
pattern LossOnFire = JcciTitles T.LossOnFire
pattern LossOnRetirementOfFixedAssets = JcciTitles T.LossOnRetirementOfFixedAssets
pattern LossOnReductionOfFixedAssets = JcciTitles T.LossOnReductionOfFixedAssets
pattern AdditionalIncomeTaxesForPriorPeriods = JcciTitles T.AdditionalIncomeTaxesForPriorPeriods
pattern RefundOfIncomeTaxes = JcciTitles T.RefundOfIncomeTaxes
pattern PurchaseRebates = JcciTitles T.PurchaseRebates
pattern WelfareExpenses = JcciTitles T.WelfareExpenses
pattern MaintenanceExpenses = JcciTitles T.MaintenanceExpenses
pattern StatutoryWelfareExpenses = JcciTitles T.StatutoryWelfareExpenses
pattern LandRentPaid = JcciTitles T.LandRentPaid
pattern InsuranceExpense = JcciTitles T.InsuranceExpense
pattern RepairsExpense = JcciTitles T.RepairsExpense
pattern StorageExpenses = JcciTitles T.StorageExpenses
pattern MembershipFees = JcciTitles T.MembershipFees
pattern IncomeSummary = JcciTitles T.IncomeSummary
pattern SuspenseAccount = JcciTitles T.SuspenseAccount
pattern ForeignExchangeGains = JcciTitles T.ForeignExchangeGains
pattern ForeignExchangeLosses = JcciTitles T.ForeignExchangeLosses
pattern ContraAccountForGuaranteeObligations = JcciTitles T.ContraAccountForGuaranteeObligations
pattern GuaranteeObligations = JcciTitles T.GuaranteeObligations
pattern IncomeTaxesAdjustment = JcciTitles T.IncomeTaxesAdjustment
pattern BranchCurrentAccount = JcciTitles T.BranchCurrentAccount
pattern HeadOfficeCurrentAccount = JcciTitles T.HeadOfficeCurrentAccount
pattern NetIncomeAttributableToNCI = JcciTitles T.NetIncomeAttributableToNCI
pattern NetLossAttributableToNCI = JcciTitles T.NetLossAttributableToNCI
pattern TradingSecurities = JcciTitles T.TradingSecurities
pattern HeldToMaturityBonds = JcciTitles T.HeldToMaturityBonds
pattern SubsidiaryStocks = JcciTitles T.SubsidiaryStocks
pattern AffiliateStocks = JcciTitles T.AffiliateStocks
pattern AvailableForSaleSecurities = JcciTitles T.AvailableForSaleSecurities
pattern ConsumptionTaxRefundReceivable = JcciTitles T.ConsumptionTaxRefundReceivable
pattern PropertyTaxPayable = JcciTitles T.PropertyTaxPayable
pattern DepositsReceivedFromOfficers = JcciTitles T.DepositsReceivedFromOfficers
pattern EntertainmentExpenses = JcciTitles T.EntertainmentExpenses
pattern MeetingExpenses = JcciTitles T.MeetingExpenses
pattern NewspaperBooksExpenses = JcciTitles T.NewspaperBooksExpenses
pattern RawMaterials = JcciTitles T.RawMaterials
pattern GoodsInTransit = JcciTitles T.GoodsInTransit
pattern SalesAllowances = JcciTitles T.SalesAllowances
pattern PurchaseAllowances = JcciTitles T.PurchaseAllowances
pattern ReversalOfAllowanceForBonuses = JcciTitles T.ReversalOfAllowanceForBonuses
pattern DirectorsCompensations = JcciTitles T.DirectorsCompensations

-- | Wildcard for a JCCI account title.
pattern AnyJcci :: JcciTitles
pattern AnyJcci = JcciTitles T.AccountTitle

{-# COMPLETE Cash
    , Deposits
    , CurrentDeposits
    , Securities
    , InvestmentSecurities
    , InvestmentInAssociate
    , LongTermNationalBonds
    , ShortTermNationalBonds
    , Products
    , Machinery
    , Building
    , Vehicle
    , StockInvestment
    , EquipmentInvestment
    , LongTermLoansReceivable
    , AccountsReceivable
    , ShortTermLoansReceivable
    , Gold
    , CapitalStock
    , RetainedEarnings
    , LongTermLoansPayable
    , ShortTermLoansPayable
    , LoansPayable
    , LongTermNationalBondsPayable
    , ShortTermNationalBondsPayable
    , Depreciation
    , AmortizationExpense
    , SalesCost
    , BusinessTrip
    , UtilitiesExpense
    , RentExpense
    , AdvertisingExpense
    , DeliveryExpenses
    , SuppliesExpenses
    , MiscellaneousExpenses
    , WageExpenditure
    , InterestExpense
    , TaxesExpense
    , Purchases
    , NetIncome
    , NationalBondInterestEarned
    , DepositInterestEarned
    , GrossProfit
    , OrdinaryProfit
    , InterestEarned
    , ReceiptFee
    , RentalIncome
    , Sales
    , EquityInEarningsOfInvestee
    , NetLoss
    , PettyCash
    , NotesReceivable
    , ElectronicallyRecordedReceivable
    , CreditCardReceivable
    , NotesLoansReceivable
    , MerchandiseInventory
    , AdvancesPaid
    , PrepaidExpenses
    , AccruedRevenue
    , OtherReceivables
    , PaymentsOnBehalf
    , SuspensePayments
    , ConsumptionTaxPaid
    , PrepaidCorporateIncomeTaxes
    , Land
    , Fixtures
    , Patent
    , Trademark
    , Software
    , CashOverShort
    , AccountsPayable
    , NotesPayable
    , ElectronicallyRecordedObligations
    , NotesLoansPayable
    , BankOverdraft
    , AdvancesReceived
    , UnearnedRevenue
    , AccruedExpenses
    , OtherPayables
    , DepositsReceived
    , SuspenseReceipts
    , ConsumptionTaxReceived
    , AccruedConsumptionTax
    , AccruedCorporateIncomeTaxes
    , UnpaidDividends
    , AllowanceForDoubtfulAccounts
    , AccumulatedDepreciation
    , LegalRetainedEarnings
    , CumulativeTranslationAdjustment
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
    , TimeDeposits
    , LoansReceivable
    , GiftCertificatesReceived
    , SecurityDepositsPaid
    , SuppliesOnHand
    , ContractAssets
    , IncomeTaxesRefundReceivable
    , WorkInProcess
    , DeferredTaxAssets
    , LeasedAssets
    , ToolsAndInstruments
    , ConstructionInProgress
    , Goodwill
    , SoftwareInProgress
    , LongTermPrepaidExpenses
    , DishonoredNotesReceivable
    , PrepaidPensionCost
    , NetDefinedBenefitAsset
    , DepositsInSpecialAccounts
    , Structures
    , LeaseholdRights
    , NonOperatingNotesReceivable
    , NonOperatingElectronicallyRecordedReceivable
    , RefundLiabilities
    , NonOperatingNotesPayable
    , NonOperatingElectronicallyRecordedObligations
    , BonusesPayable
    , AllowanceForRepairs
    , AllowanceForProductWarranties
    , AllowanceForBonuses
    , DeferredTaxLiabilities
    , LeaseObligations
    , GuaranteeDepositsReceived
    , AllowanceForRetirementBenefits
    , LongTermOtherPayables
    , NetDefinedBenefitLiability
    , StockSubscriptionDeposits
    , LegalCapitalSurplus
    , OtherCapitalSurplus
    , DividendEqualizationReserve
    , RepairFundReserve
    , ConstructionFundReserve
    , GeneralReserve
    , ValuationDifferenceOnOtherSecurities
    , NonControllingInterests
    , CapitalSurplus
    , EarnedSurplus
    , ServiceRevenue
    , OperatingRevenue
    , GainOnSalesOfSecurities
    , GainOnValuationOfSecurities
    , DividendsReceived
    , InterestOnSecurities
    , GainOnSalesOfInvestmentSecurities
    , InsuranceGain
    , GainOnBargainPurchase
    , ReversalOfAllowanceForRepairs
    , ReversalOfAllowanceForProductWarranties
    , GainOnDonationOfFixedAssets
    , GainOnNationalSubsidies
    , GainOnConstructionGrants
    , LandRentReceived
    , SalesRebates
    , CostOfServices
    , OperatingExpenses
    , InventoryShrinkageLoss
    , LossOnValuationOfMerchandise
    , Bonuses
    , RetirementBenefitExpenses
    , ProvisionForRepairs
    , ProvisionForBonuses
    , ProvisionForProductWarranties
    , ResearchAndDevelopmentExpenses
    , AmortizationOfGoodwill
    , AmortizationOfSoftware
    , AmortizationOfPatents
    , LeaseExpenses
    , IncorporationExpenses
    , StockIssuanceCosts
    , BusinessCommencementExpenses
    , DevelopmentExpenses
    , LossOnSalesOfElectronicallyRecordedReceivables
    , LossOnSalesOfReceivables
    , LossOnSalesOfSecurities
    , LossOnValuationOfSecurities
    , LossOnSalesOfInvestmentSecurities
    , LossOnFire
    , LossOnRetirementOfFixedAssets
    , LossOnReductionOfFixedAssets
    , AdditionalIncomeTaxesForPriorPeriods
    , RefundOfIncomeTaxes
    , PurchaseRebates
    , WelfareExpenses
    , MaintenanceExpenses
    , StatutoryWelfareExpenses
    , LandRentPaid
    , InsuranceExpense
    , RepairsExpense
    , StorageExpenses
    , MembershipFees
    , IncomeSummary
    , SuspenseAccount
    , ForeignExchangeGains
    , ForeignExchangeLosses
    , ContraAccountForGuaranteeObligations
    , GuaranteeObligations
    , IncomeTaxesAdjustment
    , BranchCurrentAccount
    , HeadOfficeCurrentAccount
    , NetIncomeAttributableToNCI
    , NetLossAttributableToNCI
    , TradingSecurities
    , HeldToMaturityBonds
    , SubsidiaryStocks
    , AffiliateStocks
    , AvailableForSaleSecurities
    , ConsumptionTaxRefundReceivable
    , PropertyTaxPayable
    , DepositsReceivedFromOfficers
    , EntertainmentExpenses
    , MeetingExpenses
    , NewspaperBooksExpenses
    , RawMaterials
    , GoodsInTransit
    , SalesAllowances
    , PurchaseAllowances
    , ReversalOfAllowanceForBonuses
    , DirectorsCompensations
    , AnyJcci :: JcciTitles
    #-}
