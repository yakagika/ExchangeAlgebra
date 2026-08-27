{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}
-- The exhaustive registry intentionally retains the deprecated Commutation
-- constructor so that legacy behaviour remains byte-for-byte stable.
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : ExchangeAlgebra.Algebra.Base.Account.Registry
Description : Canonical metadata registry for concrete account titles.

The exhaustive 'accountSpec' case is the single source of truth for account
classification, fixed/current status, and bilingual descriptions. 'accountAliases'
combines its base aliases with the frozen JCCI 2022 standard/permitted-name overlay.
-}
module ExchangeAlgebra.Algebra.Base.Account.Registry
    ( AccountSpec(..)
    , AccountSemantics(..)
    , accountAliases
    , accountSpec
    , accountSemantics
    , accountSpecMap
    , concreteAccountTitles
    , classifyAccountContra
    , accountDescriptions
    ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)

import ExchangeAlgebra.Algebra.Base.Account.Types
    ( AccountDivision(..), AccountRole(..), ClosingRule(..)
    , DivisionSemantics(..), FixedCurrent(..), HomeSideSemantics(..)
    , PostingCapability(..), ReportingEligibility(..), Side(..) )
import ExchangeAlgebra.Algebra.Base.Account.JcciAliases (jcciAliases)
import ExchangeAlgebra.Algebra.Base.Element (AccountTitles(..))

-- | All metadata attached to one concrete account title.
data AccountSpec = AccountSpec
    { asDivision    :: AccountDivision
    , asClosing     :: ClosingRule
      -- ^ Automatic final-stock closing policy. 'NetIncome' and 'NetLoss' are
      -- __permanent__ explicit 'NoClose' overrides (adjudicated 2026-08-11):
      -- their division encodes the P\/L /presentation side/ (profit sits on
      -- the debit side to balance the statement), so the division-derived
      -- rule would invert the transfer sign. Their closing is owned by the
      -- dedicated net-income transfer pipeline in
      -- "ExchangeAlgebra.Algebra.Transfer" (correct signs, engine-inserted
      -- balancing postings).
    , asIsContra    :: Bool
    , asFixedCurrent :: FixedCurrent
    , asNameEn      :: Text
    , asNameJa      :: Text
    , asDescription :: Text
    -- | Registry-local aliases only. Use 'accountAliases' when constructing a
    -- parser or UI: that function also includes the frozen JCCI overlay.
    , asAliases     :: [Text]
    } deriving (Show, Eq)

-- | Processing and reporting semantics attached to a concrete account title.
--
-- This record deliberately does not replace 'AccountSpec.asDivision'. The
-- legacy division remains the exchange-algebra direction input, while this
-- layer states whether that value is a financial-statement classification,
-- a bookkeeping control class, or only an internal direction encoding.
data AccountSemantics = AccountSemantics
    { asemRoles                :: [AccountRole]
    , asemPostingCapability    :: PostingCapability
    , asemDivisionSemantics    :: DivisionSemantics
    , asemHomeSideSemantics    :: HomeSideSemantics
    , asemReportingEligibility :: ReportingEligibility
    } deriving (Show, Eq)

-- | All concrete account titles in their stable Enum order.
concreteAccountTitles :: [AccountTitles]
concreteAccountTitles = filter (/= AccountTitle) [minBound .. maxBound]

-- | Look up processing and reporting semantics for a concrete account title.
-- The wildcard 'AccountTitle' is outside the metadata domain and returns
-- 'Nothing' explicitly.
accountSemantics :: AccountTitles -> Maybe AccountSemantics
accountSemantics title = do
    spec <- accountSpec title
    pure AccountSemantics
        { asemRoles = rolesFor title spec
        , asemPostingCapability = postingFor title
        , asemDivisionSemantics = divisionFor title spec
        , asemHomeSideSemantics = homeSideFor title spec
        , asemReportingEligibility = reportingFor title
        }

rolesFor :: AccountTitles -> AccountSpec -> [AccountRole]
rolesFor title spec = case title of
    NetIncome                    -> [PeriodResult]
    NetLoss                      -> [PeriodResult]
    GrossProfit                  -> [ReportingSubtotal]
    OrdinaryProfit               -> [ReportingSubtotal]
    IncomeSummary                -> [ClosingDevice]
    BranchCurrentAccount         -> [ReciprocalAccount]
    HeadOfficeCurrentAccount     -> [ReciprocalAccount]
    SuspensePayments             -> [SuspenseOrClearingAccount]
    SuspenseReceipts             -> [SuspenseOrClearingAccount]
    CashOverShort                -> [SuspenseOrClearingAccount]
    SuspenseAccount              -> [SuspenseOrClearingAccount]
    NonControllingInterests      -> [AttributionAccount]
    NetIncomeAttributableToNCI   -> [AttributionAccount, PeriodResult]
    NetLossAttributableToNCI     -> [AttributionAccount, PeriodResult]
    _ | asIsContra spec          -> [OrdinaryAccount, ContraAccount]
      | otherwise                -> [OrdinaryAccount]

postingFor :: AccountTitles -> PostingCapability
postingFor title = case title of
    NetIncome                    -> EngineGeneratedOnly
    NetLoss                      -> EngineGeneratedOnly
    GrossProfit                  -> EngineGeneratedOnly
    OrdinaryProfit               -> EngineGeneratedOnly
    IncomeSummary                -> ClosingOnly
    NonControllingInterests      -> ConsolidationOnly
    NetIncomeAttributableToNCI   -> ConsolidationOnly
    NetLossAttributableToNCI     -> ConsolidationOnly
    AccountTitle                 -> NotPostable
    _                            -> OrdinaryPosting

divisionFor :: AccountTitles -> AccountSpec -> DivisionSemantics
divisionFor title spec = case title of
    NetIncome                    -> direction
    NetLoss                      -> direction
    GrossProfit                  -> direction
    OrdinaryProfit               -> direction
    IncomeSummary                -> direction
    NetIncomeAttributableToNCI   -> direction
    NetLossAttributableToNCI     -> direction
    BranchCurrentAccount         -> control
    HeadOfficeCurrentAccount     -> control
    SuspensePayments             -> control
    SuspenseReceipts             -> control
    CashOverShort                -> control
    SuspenseAccount              -> control
    AccountTitle                 -> NoStatementDivision
    _                            -> StatementDivision (asDivision spec)
  where
    direction = DirectionEncoding (asDivision spec)
    control = BookkeepingControlClass (asDivision spec)

homeSideFor :: AccountTitles -> AccountSpec -> HomeSideSemantics
homeSideFor title spec = case title of
    IncomeSummary  -> ContextDependentHomeSide
    CashOverShort  -> ContextDependentHomeSide
    SuspenseAccount -> ContextDependentHomeSide
    AccountTitle   -> NoPostingSide
    _              -> FixedHomeSide (legacyHomeSide spec)

legacyHomeSide :: AccountSpec -> Side
legacyHomeSide spec
    | asIsContra spec = reverseSide (divisionSide (asDivision spec))
    | otherwise       = divisionSide (asDivision spec)

divisionSide :: AccountDivision -> Side
divisionSide Assets    = Debit
divisionSide Cost      = Debit
divisionSide Equity    = Credit
divisionSide Liability = Credit
divisionSide Revenue   = Credit

reverseSide :: Side -> Side
reverseSide Debit  = Credit
reverseSide Credit = Debit
reverseSide Side   = Side

reportingFor :: AccountTitles -> ReportingEligibility
reportingFor title = case title of
    NetIncome                    -> DerivedPresentation
    NetLoss                      -> DerivedPresentation
    GrossProfit                  -> DerivedPresentation
    OrdinaryProfit               -> DerivedPresentation
    IncomeSummary                -> NotPresented
    CashOverShort                -> NotPresented
    BranchCurrentAccount         -> ContextualPresentation
    HeadOfficeCurrentAccount     -> ContextualPresentation
    SuspensePayments             -> ContextualPresentation
    SuspenseReceipts             -> ContextualPresentation
    SuspenseAccount              -> ContextualPresentation
    NonControllingInterests      -> ContextualPresentation
    NetIncomeAttributableToNCI   -> ContextualPresentation
    NetLossAttributableToNCI     -> ContextualPresentation
    AccountTitle                 -> NotPresented
    _                            -> StatementEligible

-- | Every accepted non-canonical alias for an account. The JCCI overlay is
-- generated from the frozen 2022 standard/permitted account-name fixture;
-- shared permitted names intentionally remain shared so parsing reports an
-- explicit ambiguity instead of silently choosing one account.
accountAliases :: AccountTitles -> [Text]
accountAliases title = L.nub (baseAliases <> jcciAliases title)
  where
    baseAliases = maybe [] asAliases (accountSpec title)

-- | Look up metadata for an account title. The wildcard has no metadata.
--
-- Complexity: O(1)
{-# INLINE accountSpec #-}
accountSpec :: AccountTitles -> Maybe AccountSpec
accountSpec Cash = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Cash"
    , asNameJa = "現金"
    , asDescription = "Asset: Cash (現金)"
    , asAliases = ["現金"]
    }
accountSpec Deposits = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Savings deposits"
    , asNameJa = "普通預金"
    , asDescription = "Asset: Savings deposits (普通預金)"
    , asAliases = ["普通預金"]
    }
accountSpec CurrentDeposits = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Current deposits"
    , asNameJa = "当座預金"
    , asDescription = "Asset: Current deposits (当座預金)"
    , asAliases = ["当座預金"]
    }
accountSpec Securities = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Securities"
    , asNameJa = "有価証券"
    , asDescription = "Asset: Securities (有価証券)"
    , asAliases = ["有価証券"]
    }
accountSpec InvestmentSecurities = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Investment securities"
    , asNameJa = "投資有価証券"
    , asDescription = "Asset: Investment securities (投資有価証券)"
    , asAliases = ["投資有価証券"]
    }
accountSpec InvestmentInAssociate = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Investment in associate"
    , asNameJa = "関係会社株式"
    , asDescription = "Asset: Investment in associate (関係会社株式). Carrying amount under the equity method (持分法適用投資勘定)."
    , asAliases = []
    }
accountSpec LongTermNationalBonds = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term national bonds"
    , asNameJa = "長期国債"
    , asDescription = "Asset: Long-term national bonds (長期国債)"
    , asAliases = ["長期国債"]
    }
accountSpec ShortTermNationalBonds = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Short-term national bonds"
    , asNameJa = "短期国債"
    , asDescription = "Asset: Short-term national bonds (短期国債)"
    , asAliases = ["短期国債"]
    }
accountSpec Products = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Products"
    , asNameJa = "商品"
    , asDescription = "Asset: Products (商品)。分記法用。3 分法 (仕入\\/売上\\/繰越商品) では 'MerchandiseInventory' を使う"
    , asAliases = ["商品"]
    }
accountSpec Machinery = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Machinery and equipment"
    , asNameJa = "機械装置"
    , asDescription = "Asset: Machinery and equipment (機械装置)"
    , asAliases = ["機械装置"]
    }
accountSpec Building = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Real estate"
    , asNameJa = "建物"
    , asDescription = "Asset: Real estate (建物)"
    , asAliases = ["建物"]
    }
accountSpec Vehicle = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Vehicles"
    , asNameJa = "車両運搬具"
    , asDescription = "Asset: Vehicles (車両運搬具)"
    , asAliases = ["車両運搬具"]
    }
accountSpec StockInvestment = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Stock investment"
    , asNameJa = "株式投資"
    , asDescription = "Asset: Stock investment (株式投資)"
    , asAliases = ["株式投資"]
    }
accountSpec EquipmentInvestment = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Equipment investment"
    , asNameJa = "設備投資"
    , asDescription = "Asset: Equipment investment (設備投資)"
    , asAliases = ["設備投資"]
    }
accountSpec LongTermLoansReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term loans receivable"
    , asNameJa = "長期貸付金"
    , asDescription = "Asset: Long-term loans receivable (長期貸付金)"
    , asAliases = ["長期貸付金"]
    }
accountSpec AccountsReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accounts receivable"
    , asNameJa = "売掛金"
    , asDescription = "Asset: Accounts receivable (売掛金)"
    , asAliases = ["売掛金", "a/r", "accounts receivable"]
    }
accountSpec ShortTermLoansReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Short-term loans receivable"
    , asNameJa = "短期貸付金"
    , asDescription = "Asset: Short-term loans receivable (短期貸付金)"
    , asAliases = ["短期貸付金"]
    }
accountSpec ReserveDepositReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Reserve deposits, asset side"
    , asNameJa = "準備預金, 資産側 — 市中銀行が中央銀行に置く準備預金。SNA\\/マクロ系"
    , asDescription = "Asset: Reserve deposits, asset side (準備預金, 資産側 — 市中銀行が中央銀行に置く準備預金。SNA\\/マクロ系)"
    , asAliases = ["準備預金"]
    }
accountSpec Gold = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Gold"
    , asNameJa = "金"
    , asDescription = "Asset: Gold (金)"
    , asAliases = ["金"]
    }
accountSpec GovernmentService = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Government service"
    , asNameJa = "政府サービス。SNA\\/マクロ系"
    , asDescription = "Asset: Government service (政府サービス。SNA\\/マクロ系)"
    , asAliases = ["政府サービス"]
    }
accountSpec CapitalStock = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Capital stock"
    , asNameJa = "資本金"
    , asDescription = "Equity: Capital stock (資本金)"
    , asAliases = ["資本金"]
    }
accountSpec RetainedEarnings = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Retained earnings"
    , asNameJa = "繰越利益剰余金"
    , asDescription = "Equity: Retained earnings (繰越利益剰余金)"
    , asAliases = ["繰越利益剰余金"]
    }
accountSpec LongTermLoansPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term loans payable"
    , asNameJa = "長期借入金"
    , asDescription = "Liability: Long-term loans payable (長期借入金)"
    , asAliases = ["長期借入金"]
    }
accountSpec ShortTermLoansPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Short-term loans payable"
    , asNameJa = "短期借入金"
    , asDescription = "Liability: Short-term loans payable (短期借入金)"
    , asAliases = ["短期借入金"]
    }
accountSpec LoansPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Loans payable"
    , asNameJa = "借入金"
    , asDescription = "Liability: Loans payable (借入金)"
    , asAliases = ["借入金"]
    }
accountSpec ReserveForDepreciation = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Reserve for depreciation"
    , asNameJa = "減価償却引当金 — SNA\\/マクロ系の旧称。簿記の間接法には 'AccumulatedDepreciation' (減価償却累計額"
    , asDescription = "Liability: Reserve for depreciation (減価償却引当金 — SNA\\/マクロ系の旧称。簿記の間接法には 'AccumulatedDepreciation' (減価償却累計額) を使う)"
    , asAliases = ["減価償却引当金"]
    }
accountSpec DepositPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Deposits accepted"
    , asNameJa = "受入預金 — 銀行側の負債としての預金。SNA\\/マクロ系。従業員等からの預り金は 'DepositsReceived'"
    , asDescription = "Liability: Deposits accepted (受入預金 — 銀行側の負債としての預金。SNA\\/マクロ系。従業員等からの預り金は 'DepositsReceived')"
    , asAliases = ["受入預金"]
    }
accountSpec LongTermNationalBondsPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term national bonds payable"
    , asNameJa = "長期国債, 発行側"
    , asDescription = "Liability: Long-term national bonds payable (長期国債, 発行側)"
    , asAliases = ["長期国債"]
    }
accountSpec ShortTermNationalBondsPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Short-term national bonds payable"
    , asNameJa = "短期国債, 発行側"
    , asDescription = "Liability: Short-term national bonds payable (短期国債, 発行側)"
    , asAliases = ["短期国債"]
    }
accountSpec ReserveDepositPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Reserve deposits, liability side"
    , asNameJa = "準備預金, 負債側 — 中央銀行が受け入れる準備預金。SNA\\/マクロ系"
    , asDescription = "Liability: Reserve deposits, liability side (準備預金, 負債側 — 中央銀行が受け入れる準備預金。SNA\\/マクロ系)。※簿記の買掛金は 'AccountsPayable' を使うこと (旧 examples が本科目を買掛金の代用にしていた経緯あり)"
    , asAliases = ["準備預金"]
    }
accountSpec CentralBankNotePayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Central bank notes"
    , asNameJa = "発行銀行券。SNA\\/マクロ系"
    , asDescription = "Liability: Central bank notes (発行銀行券。SNA\\/マクロ系)"
    , asAliases = ["発行銀行券"]
    }
accountSpec Depreciation = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Depreciation"
    , asNameJa = "減価償却費"
    , asDescription = "Expense: Depreciation (減価償却費)"
    , asAliases = ["減価償却費"]
    }
accountSpec AmortizationExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Amortization expense for intangibles"
    , asNameJa = "無形固定資産償却費 — 特許権・商標権・ソフトウェア等の無形資産の償却費。有形の 'Depreciation' (減価償却費"
    , asDescription = "Expense: Amortization expense for intangibles (無形固定資産償却費 — 特許権・商標権・ソフトウェア等の無形資産の償却費。有形の 'Depreciation' (減価償却費) と区別される)"
    , asAliases = []
    }
accountSpec SalesCost = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Cost of sales"
    , asNameJa = "売上原価"
    , asDescription = "Expense: Cost of sales (売上原価)"
    , asAliases = ["売上原価", "cogs", "cost of sales"]
    }
accountSpec BusinessTrip = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Travel and transportation"
    , asNameJa = "旅費交通費"
    , asDescription = "Expense: Travel and transportation (旅費交通費)"
    , asAliases = ["旅費交通費"]
    }
accountSpec Commutation = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Communication"
    , asNameJa = "通信費 — 旧称。新規コードでは 'CommunicationExpenses' を使う"
    , asDescription = "Expense: Communication (通信費 — 旧称。新規コードでは 'CommunicationExpenses' を使う)"
    , asAliases = ["通信費"]
    }
accountSpec UtilitiesExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Utilities"
    , asNameJa = "水道光熱費"
    , asDescription = "Expense: Utilities (水道光熱費)"
    , asAliases = ["水道光熱費"]
    }
accountSpec RentExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Rent"
    , asNameJa = "支払家賃"
    , asDescription = "Expense: Rent (支払家賃)"
    , asAliases = ["支払家賃"]
    }
accountSpec AdvertisingExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Advertising"
    , asNameJa = "広告宣伝費"
    , asDescription = "Expense: Advertising (広告宣伝費)"
    , asAliases = ["広告宣伝費"]
    }
accountSpec DeliveryExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Delivery"
    , asNameJa = "発送費"
    , asDescription = "Expense: Delivery (発送費)"
    , asAliases = ["発送費"]
    }
accountSpec SuppliesExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Supplies"
    , asNameJa = "消耗品費"
    , asDescription = "Expense: Supplies (消耗品費)"
    , asAliases = ["消耗品費"]
    }
accountSpec MiscellaneousExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Miscellaneous"
    , asNameJa = "雑費"
    , asDescription = "Expense: Miscellaneous (雑費)"
    , asAliases = ["雑費"]
    }
accountSpec WageExpenditure = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Wages"
    , asNameJa = "給料"
    , asDescription = "Expense: Wages (給料)"
    , asAliases = ["給料"]
    }
accountSpec InterestExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Interest expense"
    , asNameJa = "支払利息"
    , asDescription = "Expense: Interest expense (支払利息)"
    , asAliases = ["支払利息"]
    }
accountSpec TaxesExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Taxes"
    , asNameJa = "租税公課"
    , asDescription = "Expense: Taxes (租税公課)"
    , asAliases = ["租税公課"]
    }
accountSpec ConsumptionExpenditure = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Consumption expenditure"
    , asNameJa = "消費支出。SNA\\/マクロ系"
    , asDescription = "Expense: Consumption expenditure (消費支出。SNA\\/マクロ系)"
    , asAliases = ["消費支出"]
    }
accountSpec SubsidyExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Subsidy expenditure"
    , asNameJa = "補助金支出。SNA\\/マクロ系"
    , asDescription = "Expense: Subsidy expenditure (補助金支出。SNA\\/マクロ系)"
    , asAliases = ["補助金支出"]
    }
accountSpec CentralBankPaymentExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Central bank payment to treasury"
    , asNameJa = "国庫納付金支出。SNA\\/マクロ系"
    , asDescription = "Expense: Central bank payment to treasury (国庫納付金支出。SNA\\/マクロ系)"
    , asAliases = ["国庫納付金支出"]
    }
accountSpec Purchases = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Purchases"
    , asNameJa = "仕入"
    , asDescription = "Expense: Purchases (仕入)"
    , asAliases = ["仕入"]
    }
accountSpec NetIncome = Just AccountSpec
    { asDivision = Cost
    , asClosing = NoClose
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Net income"
    , asNameJa = "当期純利益 — 決算振替用。借方側に立つため Expense 区分"
    , asDescription = "Expense: Net income (当期純利益 — 決算振替用。借方側に立つため Expense 区分)"
    , asAliases = ["当期純利益"]
    }
accountSpec ValueAdded = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Value added"
    , asNameJa = "付加価値。SNA\\/マクロ系"
    , asDescription = "Revenue: Value added (付加価値。SNA\\/マクロ系)"
    , asAliases = ["付加価値"]
    }
accountSpec SubsidyIncome = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Subsidy income"
    , asNameJa = "補助金収入。SNA\\/マクロ系"
    , asDescription = "Revenue: Subsidy income (補助金収入。SNA\\/マクロ系)"
    , asAliases = ["補助金収入"]
    }
accountSpec NationalBondInterestEarned = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "National bond interest earned"
    , asNameJa = "国債利息収入"
    , asDescription = "Revenue: National bond interest earned (国債利息収入)"
    , asAliases = ["国債利息収入"]
    }
accountSpec DepositInterestEarned = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Deposit interest earned"
    , asNameJa = "預金利息収入"
    , asDescription = "Revenue: Deposit interest earned (預金利息収入)"
    , asAliases = ["預金利息収入"]
    }
accountSpec GrossProfit = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gross profit"
    , asNameJa = "売上総利益 — 決算振替用"
    , asDescription = "Revenue: Gross profit (売上総利益 — 決算振替用)"
    , asAliases = ["売上総利益"]
    }
accountSpec OrdinaryProfit = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Ordinary profit"
    , asNameJa = "経常利益 — 決算振替用"
    , asDescription = "Revenue: Ordinary profit (経常利益 — 決算振替用)"
    , asAliases = ["経常利益"]
    }
accountSpec InterestEarned = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Interest earned"
    , asNameJa = "受取利息"
    , asDescription = "Revenue: Interest earned (受取利息)"
    , asAliases = ["受取利息"]
    }
accountSpec ReceiptFee = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Receipt fee"
    , asNameJa = "受取手数料。支払側は 'PaymentFees'"
    , asDescription = "Revenue: Receipt fee (受取手数料。支払側は 'PaymentFees')"
    , asAliases = ["受取手数料"]
    }
accountSpec RentalIncome = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Rental income"
    , asNameJa = "受取家賃"
    , asDescription = "Revenue: Rental income (受取家賃)"
    , asAliases = ["受取家賃"]
    }
accountSpec WageEarned = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Wage income"
    , asNameJa = "賃金収入。SNA\\/マクロ系"
    , asDescription = "Revenue: Wage income (賃金収入。SNA\\/マクロ系)"
    , asAliases = ["賃金収入"]
    }
accountSpec TaxesRevenue = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Tax revenue"
    , asNameJa = "租税収入。SNA\\/マクロ系"
    , asDescription = "Revenue: Tax revenue (租税収入。SNA\\/マクロ系)"
    , asAliases = ["租税収入"]
    }
accountSpec CentralBankPaymentIncome = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Central bank payment to treasury"
    , asNameJa = "国庫納付金収入。SNA\\/マクロ系"
    , asDescription = "Revenue: Central bank payment to treasury (国庫納付金収入。SNA\\/マクロ系)"
    , asAliases = ["国庫納付金収入"]
    }
accountSpec Sales = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Sales"
    , asNameJa = "売上"
    , asDescription = "Revenue: Sales (売上)"
    , asAliases = ["売上"]
    }
accountSpec EquityInEarningsOfInvestee = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Equity in earnings of investee"
    , asNameJa = "持分法による投資利益"
    , asDescription = "Revenue: Equity in earnings of investee (持分法による投資利益). Recognised under the equity method."
    , asAliases = []
    }
accountSpec NetLoss = Just AccountSpec
    { asDivision = Revenue
    , asClosing = NoClose
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Net loss"
    , asNameJa = "当期純損失 — 決算振替用。貸方側に立つため Revenue 区分"
    , asDescription = "Revenue: Net loss (当期純損失 — 決算振替用。貸方側に立つため Revenue 区分)"
    , asAliases = ["当期純損失"]
    }
accountSpec PettyCash = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Petty cash"
    , asNameJa = "小口現金"
    , asDescription = "Asset: Petty cash (小口現金)"
    , asAliases = ["小口現金"]
    }
accountSpec NotesReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Notes receivable"
    , asNameJa = "受取手形"
    , asDescription = "Asset: Notes receivable (受取手形)"
    , asAliases = ["受取手形"]
    }
accountSpec ElectronicallyRecordedReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Electronically recorded monetary claims"
    , asNameJa = "電子記録債権"
    , asDescription = "Asset: Electronically recorded monetary claims (電子記録債権)"
    , asAliases = ["電子記録債権"]
    }
accountSpec CreditCardReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Credit card receivable"
    , asNameJa = "クレジット売掛金"
    , asDescription = "Asset: Credit card receivable (クレジット売掛金)"
    , asAliases = ["クレジット売掛金"]
    }
accountSpec NotesLoansReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Loans receivable on notes"
    , asNameJa = "手形貸付金"
    , asDescription = "Asset: Loans receivable on notes (手形貸付金)"
    , asAliases = ["手形貸付金"]
    }
accountSpec MerchandiseInventory = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Merchandise inventory"
    , asNameJa = "繰越商品"
    , asDescription = "Asset: Merchandise inventory (繰越商品). Use under the periodic/3-account method (3 分法: Purchases\\/Sales\\/MerchandiseInventory). For the perpetual\\/specific-identification method (分記法) use 'Products' instead."
    , asAliases = ["繰越商品"]
    }
accountSpec AdvancesPaid = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Advances paid"
    , asNameJa = "前払金"
    , asDescription = "Asset: Advances paid (前払金)"
    , asAliases = ["前払金"]
    }
accountSpec PrepaidExpenses = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Prepaid expenses"
    , asNameJa = "前払費用"
    , asDescription = "Asset: Prepaid expenses (前払費用), deferral accrual account (経過勘定)"
    , asAliases = ["前払費用"]
    }
accountSpec AccruedRevenue = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued revenue"
    , asNameJa = "未収収益"
    , asDescription = "Asset: Accrued revenue (未収収益), deferral accrual account (経過勘定)"
    , asAliases = ["未収収益"]
    }
accountSpec OtherReceivables = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Other receivables"
    , asNameJa = "未収入金"
    , asDescription = "Asset: Other receivables (未収入金)"
    , asAliases = ["未収入金"]
    }
accountSpec PaymentsOnBehalf = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Payments made on behalf"
    , asNameJa = "立替金"
    , asDescription = "Asset: Payments made on behalf (立替金)"
    , asAliases = ["立替金"]
    }
accountSpec SuspensePayments = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Suspense payments"
    , asNameJa = "仮払金"
    , asDescription = "Asset: Suspense payments (仮払金)"
    , asAliases = ["仮払金"]
    }
accountSpec ConsumptionTaxPaid = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Consumption tax paid"
    , asNameJa = "仮払消費税"
    , asDescription = "Asset: Consumption tax paid (仮払消費税)"
    , asAliases = ["仮払消費税"]
    }
accountSpec PrepaidCorporateIncomeTaxes = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Prepaid corporate income taxes"
    , asNameJa = "仮払法人税等"
    , asDescription = "Asset: Prepaid corporate income taxes (仮払法人税等)"
    , asAliases = ["仮払法人税等"]
    }
accountSpec Land = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Land"
    , asNameJa = "土地"
    , asDescription = "Asset: Land (土地)"
    , asAliases = ["土地"]
    }
accountSpec Fixtures = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Fixtures and equipment"
    , asNameJa = "備品"
    , asDescription = "Asset: Fixtures and equipment (備品)"
    , asAliases = ["備品"]
    }
accountSpec Patent = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Patent"
    , asNameJa = "特許権"
    , asDescription = "Asset: Patent (特許権)"
    , asAliases = ["特許権"]
    }
accountSpec Trademark = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Trademark"
    , asNameJa = "商標権"
    , asDescription = "Asset: Trademark (商標権)"
    , asAliases = ["商標権"]
    }
accountSpec Software = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Software"
    , asNameJa = "ソフトウェア"
    , asDescription = "Asset: Software (ソフトウェア)"
    , asAliases = ["ソフトウェア"]
    }
accountSpec CashOverShort = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Cash over and short"
    , asNameJa = "現金過不足"
    , asDescription = "Asset: Cash over and short (現金過不足), a temporary/suspense account cleared at closing to MiscellaneousIncome\\/MiscellaneousLoss"
    , asAliases = ["現金過不足"]
    }
accountSpec AccountsPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accounts payable"
    , asNameJa = "買掛金"
    , asDescription = "Liability: Accounts payable (買掛金)"
    , asAliases = ["買掛金", "a/p", "accounts payable"]
    }
accountSpec NotesPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Notes payable"
    , asNameJa = "支払手形"
    , asDescription = "Liability: Notes payable (支払手形)"
    , asAliases = ["支払手形"]
    }
accountSpec ElectronicallyRecordedObligations = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Electronically recorded monetary obligations"
    , asNameJa = "電子記録債務"
    , asDescription = "Liability: Electronically recorded monetary obligations (電子記録債務)"
    , asAliases = ["電子記録債務"]
    }
accountSpec NotesLoansPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Loans payable on notes"
    , asNameJa = "手形借入金"
    , asDescription = "Liability: Loans payable on notes (手形借入金)"
    , asAliases = ["手形借入金"]
    }
accountSpec BankOverdraft = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Bank overdraft"
    , asNameJa = "当座借越"
    , asDescription = "Liability: Bank overdraft (当座借越)"
    , asAliases = ["当座借越"]
    }
accountSpec AdvancesReceived = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Advances received"
    , asNameJa = "前受金"
    , asDescription = "Liability: Advances received (前受金)"
    , asAliases = ["前受金"]
    }
accountSpec UnearnedRevenue = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Unearned revenue"
    , asNameJa = "前受収益"
    , asDescription = "Liability: Unearned revenue (前受収益), deferral accrual account (経過勘定)"
    , asAliases = ["前受収益"]
    }
accountSpec AccruedExpenses = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued expenses"
    , asNameJa = "未払費用"
    , asDescription = "Liability: Accrued expenses (未払費用), deferral accrual account (経過勘定)"
    , asAliases = ["未払費用"]
    }
accountSpec OtherPayables = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Other payables"
    , asNameJa = "未払金"
    , asDescription = "Liability: Other payables (未払金)"
    , asAliases = ["未払金"]
    }
accountSpec DepositsReceived = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Deposits received"
    , asNameJa = "預り金"
    , asDescription = "Liability: Deposits received (預り金)"
    , asAliases = ["預り金"]
    }
accountSpec SuspenseReceipts = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Suspense receipts"
    , asNameJa = "仮受金"
    , asDescription = "Liability: Suspense receipts (仮受金)"
    , asAliases = ["仮受金"]
    }
accountSpec ConsumptionTaxReceived = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Consumption tax received"
    , asNameJa = "仮受消費税"
    , asDescription = "Liability: Consumption tax received (仮受消費税)"
    , asAliases = ["仮受消費税"]
    }
accountSpec AccruedConsumptionTax = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued (unpaid) consumption tax"
    , asNameJa = "未払消費税"
    , asDescription = "Liability: Accrued (unpaid) consumption tax (未払消費税)"
    , asAliases = ["未払消費税"]
    }
accountSpec AccruedCorporateIncomeTaxes = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued (unpaid) corporate income taxes"
    , asNameJa = "未払法人税等"
    , asDescription = "Liability: Accrued (unpaid) corporate income taxes (未払法人税等)"
    , asAliases = ["未払法人税等"]
    }
accountSpec UnpaidDividends = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Unpaid dividends"
    , asNameJa = "未払配当金"
    , asDescription = "Liability: Unpaid dividends (未払配当金)"
    , asAliases = ["未払配当金"]
    }
accountSpec AllowanceForDoubtfulAccounts = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = True
    , asFixedCurrent = Current
    , asNameEn = "Allowance for doubtful accounts"
    , asNameJa = "貸倒引当金"
    , asDescription = "Asset (contra): Allowance for doubtful accounts (貸倒引当金), a credit-balance valuation account (評価勘定) deducted from receivables. Home side is Credit because it is a contra asset (isContra); values stay non-negative and the Hat\\/Not structure is intact. B\\/S deduction (net) presentation is the Write side's job."
    , asAliases = ["貸倒引当金"]
    }
accountSpec AccumulatedDepreciation = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = True
    , asFixedCurrent = Fixed
    , asNameEn = "Accumulated depreciation"
    , asNameJa = "減価償却累計額"
    , asDescription = "Asset (contra): Accumulated depreciation (減価償却累計額), a credit-balance valuation account (評価勘定) under the indirect method (間接法), deducted from the related depreciable assets. Home side is Credit because it is a contra asset (isContra). This is the canonical bookkeeping account for accumulated depreciation; the existing 'ReserveForDepreciation' is retained as the legacy SNA\\/macro-accounting name."
    , asAliases = ["減価償却累計額"]
    }
accountSpec LegalRetainedEarnings = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Legal (appropriated) retained earnings reserve"
    , asNameJa = "利益準備金"
    , asDescription = "Equity: Legal (appropriated) retained earnings reserve (利益準備金)"
    , asAliases = ["利益準備金"]
    }
accountSpec CumulativeTranslationAdjustment = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Cumulative translation adjustment"
    , asNameJa = "為替換算調整勘定; 在外子会社等の外貨建財務諸表の換算差額を計上する OCI/資本の部の項目"
    , asDescription = "Equity: Cumulative translation adjustment (為替換算調整勘定; 在外子会社等の外貨建財務諸表の換算差額を計上する OCI/資本の部の項目)"
    , asAliases = []
    }
accountSpec ProvisionForDoubtfulAccounts = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Provision for doubtful accounts"
    , asNameJa = "貸倒引当金繰入"
    , asDescription = "Cost: Provision for doubtful accounts (貸倒引当金繰入)"
    , asAliases = ["貸倒引当金繰入"]
    }
accountSpec BadDebtLoss = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Bad debt loss"
    , asNameJa = "貸倒損失"
    , asDescription = "Cost: Bad debt loss (貸倒損失)"
    , asAliases = ["貸倒損失"]
    }
accountSpec LossOnSalesOfFixedAssets = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on sales of fixed assets"
    , asNameJa = "固定資産売却損"
    , asDescription = "Cost: Loss on sales of fixed assets (固定資産売却損)"
    , asAliases = ["固定資産売却損"]
    }
accountSpec LossOnSalesOfNotesReceivable = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on sales of notes receivable"
    , asNameJa = "手形売却損"
    , asDescription = "Cost: Loss on sales of notes receivable (手形売却損)"
    , asAliases = ["手形売却損"]
    }
accountSpec PaymentFees = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Payment fees / fees paid"
    , asNameJa = "支払手数料"
    , asDescription = "Cost: Payment fees / fees paid (支払手数料), the debit counterpart of 'ReceiptFee'"
    , asAliases = ["支払手数料"]
    }
accountSpec MiscellaneousLoss = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Miscellaneous loss"
    , asNameJa = "雑損"
    , asDescription = "Cost: Miscellaneous loss (雑損)"
    , asAliases = ["雑損"]
    }
accountSpec CorporateIncomeTaxes = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Corporate income taxes"
    , asNameJa = "法人税等"
    , asDescription = "Cost: Corporate income taxes (法人税等)"
    , asAliases = ["法人税等"]
    }
accountSpec CommunicationExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Communication expenses"
    , asNameJa = "通信費"
    , asDescription = "Cost: Communication expenses (通信費). Explicitly named counterpart of the legacy 'Commutation' (also \"Communication\"); 'Commutation' is retained for backward compatibility."
    , asAliases = ["通信費"]
    }
accountSpec GainOnSalesOfFixedAssets = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on sales of fixed assets"
    , asNameJa = "固定資産売却益"
    , asDescription = "Revenue: Gain on sales of fixed assets (固定資産売却益)"
    , asAliases = ["固定資産売却益"]
    }
accountSpec RecoveryOfBadDebts = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Recovery of bad debts written off"
    , asNameJa = "償却債権取立益"
    , asDescription = "Revenue: Recovery of bad debts written off (償却債権取立益)"
    , asAliases = ["償却債権取立益"]
    }
accountSpec MiscellaneousIncome = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Miscellaneous income"
    , asNameJa = "雑益"
    , asDescription = "Revenue: Miscellaneous income (雑益)"
    , asAliases = ["雑益"]
    }
accountSpec ReversalOfAllowanceForDoubtfulAccounts = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Reversal of allowance for doubtful accounts"
    , asNameJa = "貸倒引当金戻入"
    , asDescription = "Revenue: Reversal of allowance for doubtful accounts (貸倒引当金戻入). Credit counterpart used by the 差額補充法/洗替法 when the estimated allowance is smaller than the existing balance (the excess of 'AllowanceForDoubtfulAccounts' is released)."
    , asAliases = ["貸倒引当金戻入"]
    }
accountSpec TimeDeposits = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Time deposits"
    , asNameJa = "定期預金"
    , asDescription = "Assets: Time deposits (定期預金)"
    , asAliases = ["定期預金"]
    }
accountSpec LoansReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Loans receivable"
    , asNameJa = "貸付金"
    , asDescription = "Assets: Loans receivable (貸付金)"
    , asAliases = ["貸付金"]
    }
accountSpec GiftCertificatesReceived = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Gift certificates received"
    , asNameJa = "受取商品券"
    , asDescription = "Assets: Gift certificates received (受取商品券)"
    , asAliases = ["受取商品券"]
    }
accountSpec SecurityDepositsPaid = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Guarantee deposits"
    , asNameJa = "差入保証金"
    , asDescription = "Assets: Guarantee deposits (差入保証金)"
    , asAliases = ["差入保証金"]
    }
accountSpec SuppliesOnHand = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Supplies"
    , asNameJa = "貯蔵品"
    , asDescription = "Assets: Supplies (貯蔵品)"
    , asAliases = ["貯蔵品"]
    }
accountSpec ContractAssets = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Contract assets"
    , asNameJa = "契約資産"
    , asDescription = "Assets: Contract assets (契約資産)"
    , asAliases = ["契約資産"]
    }
accountSpec IncomeTaxesRefundReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Income taxes refund receivable"
    , asNameJa = "未収還付法人税等"
    , asDescription = "Assets: Income taxes refund receivable (未収還付法人税等)"
    , asAliases = ["未収還付法人税等"]
    }
accountSpec WorkInProcess = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Work in process"
    , asNameJa = "仕掛品"
    , asDescription = "Assets: Work in process (仕掛品)"
    , asAliases = ["仕掛品"]
    }
accountSpec DeferredTaxAssets = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Deferred tax assets"
    , asNameJa = "繰延税金資産"
    , asDescription = "Assets: Deferred tax assets (繰延税金資産)"
    , asAliases = ["繰延税金資産"]
    }
accountSpec LeasedAssets = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Leased assets"
    , asNameJa = "リース資産"
    , asDescription = "Assets: Leased assets (リース資産)"
    , asAliases = ["リース資産"]
    }
accountSpec ToolsAndInstruments = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Tools and instruments"
    , asNameJa = "工具器具"
    , asDescription = "Assets: Tools and instruments (工具器具)"
    , asAliases = ["工具器具"]
    }
accountSpec ConstructionInProgress = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Construction in progress"
    , asNameJa = "建設仮勘定"
    , asDescription = "Assets: Construction in progress (建設仮勘定)"
    , asAliases = ["建設仮勘定"]
    }
accountSpec Goodwill = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Goodwill"
    , asNameJa = "のれん"
    , asDescription = "Assets: Goodwill (のれん)"
    , asAliases = ["のれん"]
    }
accountSpec SoftwareInProgress = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Software in progress"
    , asNameJa = "ソフトウェア仮勘定"
    , asDescription = "Assets: Software in progress (ソフトウェア仮勘定)"
    , asAliases = ["ソフトウェア仮勘定"]
    }
accountSpec LongTermPrepaidExpenses = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term prepaid expenses"
    , asNameJa = "長期前払費用"
    , asDescription = "Assets: Long-term prepaid expenses (長期前払費用)"
    , asAliases = ["長期前払費用"]
    }
accountSpec DishonoredNotesReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Dishonored notes receivable"
    , asNameJa = "不渡手形"
    , asDescription = "Assets: Dishonored notes receivable (不渡手形)"
    , asAliases = ["不渡手形"]
    }
accountSpec PrepaidPensionCost = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Prepaid pension costs"
    , asNameJa = "前払年金費用"
    , asDescription = "Assets: Prepaid pension costs (前払年金費用)"
    , asAliases = ["前払年金費用"]
    }
accountSpec NetDefinedBenefitAsset = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Retirement benefit asset"
    , asNameJa = "退職給付に係る資産"
    , asDescription = "Assets: Retirement benefit asset (退職給付に係る資産)"
    , asAliases = ["退職給付に係る資産"]
    }
accountSpec DepositsInSpecialAccounts = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Deposits in special accounts"
    , asNameJa = "別段預金"
    , asDescription = "Assets: Deposits in special accounts (別段預金)"
    , asAliases = ["別段預金"]
    }
accountSpec Structures = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Structures"
    , asNameJa = "構築物"
    , asDescription = "Assets: Structures (構築物)"
    , asAliases = ["構築物"]
    }
accountSpec LeaseholdRights = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Leasehold interests in land"
    , asNameJa = "借地権"
    , asDescription = "Assets: Leasehold interests in land (借地権)"
    , asAliases = ["借地権"]
    }
accountSpec NonOperatingNotesReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Non-operating notes receivable"
    , asNameJa = "営業外受取手形"
    , asDescription = "Assets: Non-operating notes receivable (営業外受取手形)"
    , asAliases = ["営業外受取手形"]
    }
accountSpec NonOperatingElectronicallyRecordedReceivable = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Electronically recorded monetary claims - non-operating"
    , asNameJa = "営業外電子記録債権"
    , asDescription = "Assets: Electronically recorded monetary claims - non-operating (営業外電子記録債権)"
    , asAliases = ["営業外電子記録債権"]
    }
accountSpec RefundLiabilities = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Refund liabilities"
    , asNameJa = "返金負債"
    , asDescription = "Liability: Refund liabilities (返金負債)"
    , asAliases = ["返金負債"]
    }
accountSpec NonOperatingNotesPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Non-operating notes payable"
    , asNameJa = "営業外支払手形"
    , asDescription = "Liability: Non-operating notes payable (営業外支払手形)"
    , asAliases = ["営業外支払手形"]
    }
accountSpec NonOperatingElectronicallyRecordedObligations = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Electronically recorded obligations - non-operating"
    , asNameJa = "営業外電子記録債務"
    , asDescription = "Liability: Electronically recorded obligations - non-operating (営業外電子記録債務)"
    , asAliases = ["営業外電子記録債務"]
    }
accountSpec BonusesPayable = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued bonuses"
    , asNameJa = "未払賞与"
    , asDescription = "Liability: Accrued bonuses (未払賞与)"
    , asAliases = ["未払賞与"]
    }
accountSpec AllowanceForRepairs = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Provision for repairs"
    , asNameJa = "修繕引当金"
    , asDescription = "Liability: Provision for repairs (修繕引当金)"
    , asAliases = ["修繕引当金"]
    }
accountSpec AllowanceForProductWarranties = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Provision for warranties"
    , asNameJa = "商品保証引当金"
    , asDescription = "Liability: Provision for warranties (商品保証引当金)"
    , asAliases = ["商品保証引当金"]
    }
accountSpec AllowanceForBonuses = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Provision for bonuses"
    , asNameJa = "賞与引当金"
    , asDescription = "Liability: Provision for bonuses (賞与引当金)"
    , asAliases = ["賞与引当金"]
    }
accountSpec DeferredTaxLiabilities = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Deferred tax liabilities"
    , asNameJa = "繰延税金負債"
    , asDescription = "Liability: Deferred tax liabilities (繰延税金負債)"
    , asAliases = ["繰延税金負債"]
    }
accountSpec LeaseObligations = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Lease liabilities"
    , asNameJa = "リース債務"
    , asDescription = "Liability: Lease liabilities (リース債務)"
    , asAliases = ["リース債務"]
    }
accountSpec GuaranteeDepositsReceived = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Guarantee deposits received"
    , asNameJa = "預り保証金"
    , asDescription = "Liability: Guarantee deposits received (預り保証金)"
    , asAliases = ["預り保証金"]
    }
accountSpec AllowanceForRetirementBenefits = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Provision for retirement benefits"
    , asNameJa = "退職給付引当金"
    , asDescription = "Liability: Provision for retirement benefits (退職給付引当金)"
    , asAliases = ["退職給付引当金"]
    }
accountSpec LongTermOtherPayables = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term accounts payable - other"
    , asNameJa = "長期未払金"
    , asDescription = "Liability: Long-term accounts payable - other (長期未払金)"
    , asAliases = ["長期未払金"]
    }
accountSpec NetDefinedBenefitLiability = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Retirement benefit liability"
    , asNameJa = "退職給付に係る負債"
    , asDescription = "Liability: Retirement benefit liability (退職給付に係る負債)"
    , asAliases = ["退職給付に係る負債"]
    }
accountSpec StockSubscriptionDeposits = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Deposits for subscriptions of shares"
    , asNameJa = "株式申込証拠金"
    , asDescription = "Equity: Deposits for subscriptions of shares (株式申込証拠金)"
    , asAliases = ["株式申込証拠金"]
    }
accountSpec LegalCapitalSurplus = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Legal capital surplus"
    , asNameJa = "資本準備金"
    , asDescription = "Equity: Legal capital surplus (資本準備金)"
    , asAliases = ["資本準備金"]
    }
accountSpec OtherCapitalSurplus = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Other capital surplus"
    , asNameJa = "その他資本剰余金"
    , asDescription = "Equity: Other capital surplus (その他資本剰余金)"
    , asAliases = ["その他資本剰余金"]
    }
accountSpec DividendEqualizationReserve = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Reserve for dividend equalization"
    , asNameJa = "配当平均積立金"
    , asDescription = "Equity: Reserve for dividend equalization (配当平均積立金)"
    , asAliases = ["配当平均積立金"]
    }
accountSpec RepairFundReserve = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Reserve for repairs"
    , asNameJa = "修繕積立金"
    , asDescription = "Equity: Reserve for repairs (修繕積立金)"
    , asAliases = ["修繕積立金"]
    }
accountSpec ConstructionFundReserve = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Reserve for new construction"
    , asNameJa = "新築積立金"
    , asDescription = "Equity: Reserve for new construction (新築積立金)"
    , asAliases = ["新築積立金"]
    }
accountSpec GeneralReserve = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "General reserve"
    , asNameJa = "別途積立金"
    , asDescription = "Equity: General reserve (別途積立金)"
    , asAliases = ["別途積立金"]
    }
accountSpec ValuationDifferenceOnOtherSecurities = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Valuation difference on available-for-sale securities"
    , asNameJa = "その他有価証券評価差額金"
    , asDescription = "Equity: Valuation difference on available-for-sale securities (その他有価証券評価差額金)"
    , asAliases = ["その他有価証券評価差額金"]
    }
accountSpec NonControllingInterests = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Non-controlling interests"
    , asNameJa = "非支配株主持分"
    , asDescription = "Equity: Non-controlling interests (非支配株主持分)"
    , asAliases = ["非支配株主持分"]
    }
accountSpec CapitalSurplus = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Capital surplus"
    , asNameJa = "資本剰余金"
    , asDescription = "Equity: Capital surplus (資本剰余金)"
    , asAliases = ["資本剰余金"]
    }
accountSpec EarnedSurplus = Just AccountSpec
    { asDivision = Equity
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Retained earnings"
    , asNameJa = "利益剰余金"
    , asDescription = "Equity: Retained earnings (利益剰余金)"
    , asAliases = ["利益剰余金"]
    }
accountSpec ServiceRevenue = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Service revenue"
    , asNameJa = "役務収益"
    , asDescription = "Revenue: Service revenue (役務収益)"
    , asAliases = ["役務収益"]
    }
accountSpec OperatingRevenue = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Operating revenue"
    , asNameJa = "営業収益"
    , asDescription = "Revenue: Operating revenue (営業収益)"
    , asAliases = ["営業収益"]
    }
accountSpec GainOnSalesOfSecurities = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on sale of securities"
    , asNameJa = "有価証券売却益"
    , asDescription = "Revenue: Gain on sale of securities (有価証券売却益)"
    , asAliases = ["有価証券売却益"]
    }
accountSpec GainOnValuationOfSecurities = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on valuation of securities"
    , asNameJa = "有価証券評価益"
    , asDescription = "Revenue: Gain on valuation of securities (有価証券評価益)"
    , asAliases = ["有価証券評価益"]
    }
accountSpec DividendsReceived = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Dividend income"
    , asNameJa = "受取配当金"
    , asDescription = "Revenue: Dividend income (受取配当金)"
    , asAliases = ["受取配当金"]
    }
accountSpec InterestOnSecurities = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Interest on securities"
    , asNameJa = "有価証券利息"
    , asDescription = "Revenue: Interest on securities (有価証券利息)"
    , asAliases = ["有価証券利息"]
    }
accountSpec GainOnSalesOfInvestmentSecurities = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on sale of investment securities"
    , asNameJa = "投資有価証券売却益"
    , asDescription = "Revenue: Gain on sale of investment securities (投資有価証券売却益)"
    , asAliases = ["投資有価証券売却益"]
    }
accountSpec InsuranceGain = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on insurance claims"
    , asNameJa = "保険差益"
    , asDescription = "Revenue: Gain on insurance claims (保険差益)"
    , asAliases = ["保険差益"]
    }
accountSpec GainOnBargainPurchase = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on bargain purchase"
    , asNameJa = "負ののれん発生益"
    , asDescription = "Revenue: Gain on bargain purchase (負ののれん発生益)"
    , asAliases = ["負ののれん発生益"]
    }
accountSpec ReversalOfAllowanceForRepairs = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Reversal of provision for repairs"
    , asNameJa = "修繕引当金戻入"
    , asDescription = "Revenue: Reversal of provision for repairs (修繕引当金戻入)"
    , asAliases = ["修繕引当金戻入"]
    }
accountSpec ReversalOfAllowanceForProductWarranties = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Reversal of provision for warranties"
    , asNameJa = "商品保証引当金戻入"
    , asDescription = "Revenue: Reversal of provision for warranties (商品保証引当金戻入)"
    , asAliases = ["商品保証引当金戻入"]
    }
accountSpec GainOnDonationOfFixedAssets = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on receipt of donated non-current assets"
    , asNameJa = "固定資産受贈益"
    , asDescription = "Revenue: Gain on receipt of donated non-current assets (固定資産受贈益)"
    , asAliases = ["固定資産受贈益"]
    }
accountSpec GainOnNationalSubsidies = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on receipt of national subsidies"
    , asNameJa = "国庫補助金受贈益"
    , asDescription = "Revenue: Gain on receipt of national subsidies (国庫補助金受贈益)"
    , asAliases = ["国庫補助金受贈益"]
    }
accountSpec GainOnConstructionGrants = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on contribution received for construction"
    , asNameJa = "工事負担金受贈益"
    , asDescription = "Revenue: Gain on contribution received for construction (工事負担金受贈益)"
    , asAliases = ["工事負担金受贈益"]
    }
accountSpec LandRentReceived = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Rental income from land"
    , asNameJa = "受取地代"
    , asDescription = "Revenue: Rental income from land (受取地代)"
    , asAliases = ["受取地代"]
    }
accountSpec SalesRebates = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = True
    , asFixedCurrent = Other
    , asNameEn = "Sales rebates"
    , asNameJa = "売上割戻"
    , asDescription = "Revenue: Sales rebates (売上割戻)"
    , asAliases = ["売上割戻"]
    }
accountSpec CostOfServices = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Cost of services"
    , asNameJa = "役務原価"
    , asDescription = "Cost: Cost of services (役務原価)"
    , asAliases = ["役務原価"]
    }
accountSpec OperatingExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Operating expenses"
    , asNameJa = "営業費用"
    , asDescription = "Cost: Operating expenses (営業費用)"
    , asAliases = ["営業費用"]
    }
accountSpec InventoryShrinkageLoss = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Inventory shrinkage loss"
    , asNameJa = "棚卸減耗損"
    , asDescription = "Cost: Inventory shrinkage loss (棚卸減耗損)"
    , asAliases = ["棚卸減耗損"]
    }
accountSpec LossOnValuationOfMerchandise = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Valuation loss on goods"
    , asNameJa = "商品評価損"
    , asDescription = "Cost: Valuation loss on goods (商品評価損)"
    , asAliases = ["商品評価損"]
    }
accountSpec Bonuses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Bonuses"
    , asNameJa = "賞与"
    , asDescription = "Cost: Bonuses (賞与)"
    , asAliases = ["賞与"]
    }
accountSpec RetirementBenefitExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Retirement benefit expenses"
    , asNameJa = "退職給付費用"
    , asDescription = "Cost: Retirement benefit expenses (退職給付費用)"
    , asAliases = ["退職給付費用"]
    }
accountSpec ProvisionForRepairs = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Provision for repairs"
    , asNameJa = "修繕引当金繰入"
    , asDescription = "Cost: Provision for repairs (修繕引当金繰入)"
    , asAliases = ["修繕引当金繰入"]
    }
accountSpec ProvisionForBonuses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Provision for bonuses"
    , asNameJa = "賞与引当金繰入"
    , asDescription = "Cost: Provision for bonuses (賞与引当金繰入)"
    , asAliases = ["賞与引当金繰入"]
    }
accountSpec ProvisionForProductWarranties = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Provision for warranties"
    , asNameJa = "商品保証引当金繰入"
    , asDescription = "Cost: Provision for warranties (商品保証引当金繰入)"
    , asAliases = ["商品保証引当金繰入"]
    }
accountSpec ResearchAndDevelopmentExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Research and development expenses"
    , asNameJa = "研究開発費"
    , asDescription = "Cost: Research and development expenses (研究開発費)"
    , asAliases = ["研究開発費"]
    }
accountSpec AmortizationOfGoodwill = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Amortization of goodwill"
    , asNameJa = "のれん償却"
    , asDescription = "Cost: Amortization of goodwill (のれん償却)"
    , asAliases = ["のれん償却"]
    }
accountSpec AmortizationOfSoftware = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Amortization of software"
    , asNameJa = "ソフトウェア償却"
    , asDescription = "Cost: Amortization of software (ソフトウェア償却)"
    , asAliases = ["ソフトウェア償却"]
    }
accountSpec AmortizationOfPatents = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Amortization of patent rights"
    , asNameJa = "特許権償却"
    , asDescription = "Cost: Amortization of patent rights (特許権償却)"
    , asAliases = ["特許権償却"]
    }
accountSpec LeaseExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Lease expenses"
    , asNameJa = "支払リース料"
    , asDescription = "Cost: Lease expenses (支払リース料)"
    , asAliases = ["支払リース料"]
    }
accountSpec IncorporationExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Organization expenses"
    , asNameJa = "創立費"
    , asDescription = "Cost: Organization expenses (創立費)"
    , asAliases = ["創立費"]
    }
accountSpec StockIssuanceCosts = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Share issuance costs"
    , asNameJa = "株式交付費"
    , asDescription = "Cost: Share issuance costs (株式交付費)"
    , asAliases = ["株式交付費"]
    }
accountSpec BusinessCommencementExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Business commencement expenses"
    , asNameJa = "開業費"
    , asDescription = "Cost: Business commencement expenses (開業費)"
    , asAliases = ["開業費"]
    }
accountSpec DevelopmentExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Development expenses"
    , asNameJa = "開発費"
    , asDescription = "Cost: Development expenses (開発費)"
    , asAliases = ["開発費"]
    }
accountSpec LossOnSalesOfElectronicallyRecordedReceivables = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on sale of electronically recorded monetary claims"
    , asNameJa = "電子記録債権売却損"
    , asDescription = "Cost: Loss on sale of electronically recorded monetary claims (電子記録債権売却損)"
    , asAliases = ["電子記録債権売却損"]
    }
accountSpec LossOnSalesOfReceivables = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on sale of receivables"
    , asNameJa = "債権売却損"
    , asDescription = "Cost: Loss on sale of receivables (債権売却損)"
    , asAliases = ["債権売却損"]
    }
accountSpec LossOnSalesOfSecurities = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on sale of securities"
    , asNameJa = "有価証券売却損"
    , asDescription = "Cost: Loss on sale of securities (有価証券売却損)"
    , asAliases = ["有価証券売却損"]
    }
accountSpec LossOnValuationOfSecurities = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on valuation of securities"
    , asNameJa = "有価証券評価損"
    , asDescription = "Cost: Loss on valuation of securities (有価証券評価損)"
    , asAliases = ["有価証券評価損"]
    }
accountSpec LossOnSalesOfInvestmentSecurities = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on sale of investment securities"
    , asNameJa = "投資有価証券売却損"
    , asDescription = "Cost: Loss on sale of investment securities (投資有価証券売却損)"
    , asAliases = ["投資有価証券売却損"]
    }
accountSpec LossOnFire = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on fire"
    , asNameJa = "火災損失"
    , asDescription = "Cost: Loss on fire (火災損失)"
    , asAliases = ["火災損失"]
    }
accountSpec LossOnRetirementOfFixedAssets = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on retirement of non-current assets"
    , asNameJa = "固定資産除却損"
    , asDescription = "Cost: Loss on retirement of non-current assets (固定資産除却損)"
    , asAliases = ["固定資産除却損"]
    }
accountSpec LossOnReductionOfFixedAssets = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on tax purpose reduction entry of non-current assets"
    , asNameJa = "固定資産圧縮損"
    , asDescription = "Cost: Loss on tax purpose reduction entry of non-current assets (固定資産圧縮損)"
    , asAliases = ["固定資産圧縮損"]
    }
accountSpec AdditionalIncomeTaxesForPriorPeriods = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Additional income taxes for prior periods"
    , asNameJa = "追徴法人税等"
    , asDescription = "Cost: Additional income taxes for prior periods (追徴法人税等)"
    , asAliases = ["追徴法人税等"]
    }
accountSpec RefundOfIncomeTaxes = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = True
    , asFixedCurrent = Other
    , asNameEn = "Refund of income taxes"
    , asNameJa = "還付法人税等"
    , asDescription = "Cost: Refund of income taxes (還付法人税等)"
    , asAliases = ["還付法人税等"]
    }
accountSpec PurchaseRebates = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = True
    , asFixedCurrent = Other
    , asNameEn = "Purchase rebates"
    , asNameJa = "仕入割戻"
    , asDescription = "Cost: Purchase rebates (仕入割戻)"
    , asAliases = ["仕入割戻"]
    }
accountSpec WelfareExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Welfare expenses"
    , asNameJa = "福利厚生費"
    , asDescription = "Cost: Welfare expenses (福利厚生費)"
    , asAliases = ["福利厚生費"]
    }
accountSpec MaintenanceExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Maintenance expenses"
    , asNameJa = "保守費"
    , asDescription = "Cost: Maintenance expenses (保守費)"
    , asAliases = ["保守費"]
    }
accountSpec StatutoryWelfareExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Legal welfare expenses"
    , asNameJa = "法定福利費"
    , asDescription = "Cost: Legal welfare expenses (法定福利費)"
    , asAliases = ["法定福利費"]
    }
accountSpec LandRentPaid = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Rent expenses on land"
    , asNameJa = "支払地代"
    , asDescription = "Cost: Rent expenses on land (支払地代)"
    , asAliases = ["支払地代"]
    }
accountSpec InsuranceExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Insurance expenses"
    , asNameJa = "保険料"
    , asDescription = "Cost: Insurance expenses (保険料)"
    , asAliases = ["保険料"]
    }
accountSpec RepairsExpense = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Repair expenses"
    , asNameJa = "修繕費"
    , asDescription = "Cost: Repair expenses (修繕費)"
    , asAliases = ["修繕費"]
    }
accountSpec StorageExpenses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Storage costs"
    , asNameJa = "保管費"
    , asDescription = "Cost: Storage costs (保管費)"
    , asAliases = ["保管費"]
    }
accountSpec MembershipFees = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Membership fees"
    , asNameJa = "諸会費"
    , asDescription = "Cost: Membership fees (諸会費)"
    , asAliases = ["諸会費"]
    }
accountSpec IncomeSummary = Just AccountSpec
    { asDivision = Assets
    , asClosing = NoClose
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Income summary"
    , asNameJa = "損益"
    , asDescription = "Bookkeeping device: Income summary (損益). Assets/NoClose is a technical debit-side placeholder; it is not a balance-sheet asset and is not automatically closed."
    , asAliases = ["損益"]
    }
accountSpec SuspenseAccount = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Suspense account"
    , asNameJa = "未決算"
    , asDescription = "Assets: Suspense account (未決算)"
    , asAliases = ["未決算"]
    }
accountSpec ForeignExchangeGains = Just AccountSpec
    { asDivision = Revenue
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Foreign exchange gains"
    , asNameJa = "為替差益"
    , asDescription = "Revenue: Foreign exchange gains (為替差益)"
    , asAliases = ["為替差益"]
    }
accountSpec ForeignExchangeLosses = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Foreign exchange losses"
    , asNameJa = "為替差損"
    , asDescription = "Cost: Foreign exchange losses (為替差損)"
    , asAliases = ["為替差損"]
    }
accountSpec ContraAccountForGuaranteeObligations = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Contra account for guarantee obligations"
    , asNameJa = "保証債務見返"
    , asDescription = "Assets: Contra account for guarantee obligations (保証債務見返)"
    , asAliases = ["保証債務見返"]
    }
accountSpec GuaranteeObligations = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Guarantee obligations"
    , asNameJa = "保証債務"
    , asDescription = "Liability: Guarantee obligations (保証債務)"
    , asAliases = ["保証債務"]
    }
accountSpec IncomeTaxesAdjustment = Just AccountSpec
    { asDivision = Cost
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Income taxes - deferred"
    , asNameJa = "法人税等調整額"
    , asDescription = "Cost: Income taxes - deferred (法人税等調整額)"
    , asAliases = ["法人税等調整額"]
    }
accountSpec BranchCurrentAccount = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Branch current account"
    , asNameJa = "支店"
    , asDescription = "Assets: Branch current account (支店)"
    , asAliases = ["支店"]
    }
accountSpec HeadOfficeCurrentAccount = Just AccountSpec
    { asDivision = Liability
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Head office current account"
    , asNameJa = "本店"
    , asDescription = "Liability: Head office current account (本店), the credit-balance reciprocal account in branch books."
    , asAliases = ["本店"]
    }
accountSpec NetIncomeAttributableToNCI = Just AccountSpec
    { asDivision = Cost
    , asClosing = NoClose
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Profit attributable to non-controlling interests"
    , asNameJa = "非支配株主に帰属する当期純利益"
    , asDescription = "Cost: Profit attributable to non-controlling interests (非支配株主に帰属する当期純利益). NoClose: consolidation procedures transfer it separately to non-controlling interests."
    , asAliases = ["非支配株主に帰属する当期純利益"]
    }
accountSpec NetLossAttributableToNCI = Just AccountSpec
    { asDivision = Revenue
    , asClosing = NoClose
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss attributable to non-controlling interests"
    , asNameJa = "非支配株主に帰属する当期純損失"
    , asDescription = "Revenue: Loss attributable to non-controlling interests (非支配株主に帰属する当期純損失). NoClose: consolidation procedures transfer it separately to non-controlling interests."
    , asAliases = ["非支配株主に帰属する当期純損失"]
    }
accountSpec TradingSecurities = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Trading securities"
    , asNameJa = "売買目的有価証券"
    , asDescription = "Assets: Trading securities (売買目的有価証券)"
    , asAliases = ["売買目的有価証券"]
    }
accountSpec HeldToMaturityBonds = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Held-to-maturity bonds"
    , asNameJa = "満期保有目的債券"
    , asDescription = "Assets: Held-to-maturity bonds (満期保有目的債券)"
    , asAliases = ["満期保有目的債券"]
    }
accountSpec SubsidiaryStocks = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Shares of subsidiaries"
    , asNameJa = "子会社株式"
    , asDescription = "Assets: Shares of subsidiaries (子会社株式)"
    , asAliases = ["子会社株式"]
    }
accountSpec AffiliateStocks = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Shares of associates"
    , asNameJa = "関連会社株式"
    , asDescription = "Assets: Shares of associates (関連会社株式)"
    , asAliases = ["関連会社株式"]
    }
accountSpec AvailableForSaleSecurities = Just AccountSpec
    { asDivision = Assets
    , asClosing = CloseByDivision
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Available-for-sale securities"
    , asNameJa = "その他有価証券"
    , asDescription = "Assets: Available-for-sale securities (その他有価証券)"
    , asAliases = ["その他有価証券"]
    }
accountSpec AccountTitle = Nothing

-- | Derived strict map view. The exhaustive function above remains canonical.
accountSpecMap :: Map AccountTitles AccountSpec
accountSpecMap = M.fromList
    [ (title, spec)
    | title <- concreteAccountTitles
    , Just spec <- [accountSpec title]
    ]

-- | Whether an account is a contra account.
--
-- The result is projected from 'AccountSpec.asIsContra'; the wildcard has no
-- specification and therefore returns 'False'. Complexity: O(1)
{-# INLINE classifyAccountContra #-}
classifyAccountContra :: AccountTitles -> Bool
classifyAccountContra title = maybe False asIsContra (accountSpec title)

-- | Compatibility projection used by the assistance API.
accountDescriptions :: [(AccountTitles, Text, Text, Text)]
accountDescriptions = mapMaybe project concreteAccountTitles
  where
    project title = do
        spec <- accountSpec title
        pure (title, asNameEn spec, asNameJa spec, asDescription spec)
