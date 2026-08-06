{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}
-- The exhaustive registry intentionally retains the deprecated Commutation
-- constructor so that legacy behaviour remains byte-for-byte stable.
{-# OPTIONS_GHC -Wno-deprecations #-}

{- |
Module      : ExchangeAlgebra.Algebra.Base.Account.Registry
Description : Canonical metadata registry for concrete account titles.

The exhaustive 'accountSpec' case is the single source of truth for account
classification, fixed/current status, bilingual descriptions, and aliases.
-}
module ExchangeAlgebra.Algebra.Base.Account.Registry
    ( AccountSpec(..)
    , accountSpec
    , accountSpecMap
    , concreteAccountTitles
    , classifyAccountContra
    , accountDescriptions
    ) where

import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)

import ExchangeAlgebra.Algebra.Base.Account.Types
    ( AccountDivision(..), FixedCurrent(..) )
import ExchangeAlgebra.Algebra.Base.Element (AccountTitles(..))

-- | All metadata attached to one concrete account title.
data AccountSpec = AccountSpec
    { asDivision    :: AccountDivision
    , asIsContra    :: Bool
    , asFixedCurrent :: FixedCurrent
    , asNameEn      :: Text
    , asNameJa      :: Text
    , asDescription :: Text
    , asAliases     :: [Text]
    } deriving (Show, Eq)

-- | All concrete account titles in their stable Enum order.
concreteAccountTitles :: [AccountTitles]
concreteAccountTitles = [Cash .. ReversalOfAllowanceForDoubtfulAccounts]

-- | Look up metadata for an account title. The wildcard has no metadata.
--
-- Complexity: O(1)
{-# INLINE accountSpec #-}
accountSpec :: AccountTitles -> Maybe AccountSpec
accountSpec Cash = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Cash"
    , asNameJa = "現金"
    , asDescription = "Asset: Cash (現金)"
    , asAliases = ["現金"]
    }
accountSpec Deposits = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Savings deposits"
    , asNameJa = "普通預金"
    , asDescription = "Asset: Savings deposits (普通預金)"
    , asAliases = ["普通預金"]
    }
accountSpec CurrentDeposits = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Current deposits"
    , asNameJa = "当座預金"
    , asDescription = "Asset: Current deposits (当座預金)"
    , asAliases = ["当座預金"]
    }
accountSpec Securities = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Securities"
    , asNameJa = "有価証券"
    , asDescription = "Asset: Securities (有価証券)"
    , asAliases = ["有価証券"]
    }
accountSpec InvestmentSecurities = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Investment securities"
    , asNameJa = "投資有価証券"
    , asDescription = "Asset: Investment securities (投資有価証券)"
    , asAliases = ["投資有価証券"]
    }
accountSpec InvestmentInAssociate = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Investment in associate"
    , asNameJa = "関係会社株式"
    , asDescription = "Asset: Investment in associate (関係会社株式). Carrying amount under the equity method (持分法適用投資勘定)."
    , asAliases = []
    }
accountSpec LongTermNationalBonds = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term national bonds"
    , asNameJa = "長期国債"
    , asDescription = "Asset: Long-term national bonds (長期国債)"
    , asAliases = ["長期国債"]
    }
accountSpec ShortTermNationalBonds = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Short-term national bonds"
    , asNameJa = "短期国債"
    , asDescription = "Asset: Short-term national bonds (短期国債)"
    , asAliases = ["短期国債"]
    }
accountSpec Products = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Products"
    , asNameJa = "商品"
    , asDescription = "Asset: Products (商品)。分記法用。3 分法 (仕入\\/売上\\/繰越商品) では 'MerchandiseInventory' を使う"
    , asAliases = ["商品"]
    }
accountSpec Machinery = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Machinery and equipment"
    , asNameJa = "機械装置"
    , asDescription = "Asset: Machinery and equipment (機械装置)"
    , asAliases = ["機械装置"]
    }
accountSpec Building = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Real estate"
    , asNameJa = "建物"
    , asDescription = "Asset: Real estate (建物)"
    , asAliases = ["建物"]
    }
accountSpec Vehicle = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Vehicles"
    , asNameJa = "車両運搬具"
    , asDescription = "Asset: Vehicles (車両運搬具)"
    , asAliases = ["車両運搬具"]
    }
accountSpec StockInvestment = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Stock investment"
    , asNameJa = "株式投資"
    , asDescription = "Asset: Stock investment (株式投資)"
    , asAliases = ["株式投資"]
    }
accountSpec EquipmentInvestment = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Equipment investment"
    , asNameJa = "設備投資"
    , asDescription = "Asset: Equipment investment (設備投資)"
    , asAliases = ["設備投資"]
    }
accountSpec LongTermLoansReceivable = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term loans receivable"
    , asNameJa = "長期貸付金"
    , asDescription = "Asset: Long-term loans receivable (長期貸付金)"
    , asAliases = ["長期貸付金"]
    }
accountSpec AccountsReceivable = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accounts receivable"
    , asNameJa = "売掛金"
    , asDescription = "Asset: Accounts receivable (売掛金)"
    , asAliases = ["売掛金", "a/r", "accounts receivable"]
    }
accountSpec ShortTermLoansReceivable = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Short-term loans receivable"
    , asNameJa = "短期貸付金"
    , asDescription = "Asset: Short-term loans receivable (短期貸付金)"
    , asAliases = ["短期貸付金"]
    }
accountSpec ReserveDepositReceivable = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Reserve deposits, asset side"
    , asNameJa = "準備預金, 資産側 — 市中銀行が中央銀行に置く準備預金。SNA\\/マクロ系"
    , asDescription = "Asset: Reserve deposits, asset side (準備預金, 資産側 — 市中銀行が中央銀行に置く準備預金。SNA\\/マクロ系)"
    , asAliases = ["準備預金"]
    }
accountSpec Gold = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Gold"
    , asNameJa = "金"
    , asDescription = "Asset: Gold (金)"
    , asAliases = ["金"]
    }
accountSpec GovernmentService = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Government service"
    , asNameJa = "政府サービス。SNA\\/マクロ系"
    , asDescription = "Asset: Government service (政府サービス。SNA\\/マクロ系)"
    , asAliases = ["政府サービス"]
    }
accountSpec CapitalStock = Just AccountSpec
    { asDivision = Equity
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Capital stock"
    , asNameJa = "資本金"
    , asDescription = "Equity: Capital stock (資本金)"
    , asAliases = ["資本金"]
    }
accountSpec RetainedEarnings = Just AccountSpec
    { asDivision = Equity
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Retained earnings"
    , asNameJa = "繰越利益剰余金"
    , asDescription = "Equity: Retained earnings (繰越利益剰余金)"
    , asAliases = ["繰越利益剰余金"]
    }
accountSpec LongTermLoansPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term loans payable"
    , asNameJa = "長期借入金"
    , asDescription = "Liability: Long-term loans payable (長期借入金)"
    , asAliases = ["長期借入金"]
    }
accountSpec ShortTermLoansPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Short-term loans payable"
    , asNameJa = "短期借入金"
    , asDescription = "Liability: Short-term loans payable (短期借入金)"
    , asAliases = ["短期借入金"]
    }
accountSpec LoansPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Loans payable"
    , asNameJa = "借入金"
    , asDescription = "Liability: Loans payable (借入金)"
    , asAliases = ["借入金"]
    }
accountSpec ReserveForDepreciation = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Reserve for depreciation"
    , asNameJa = "減価償却引当金 — SNA\\/マクロ系の旧称。簿記の間接法には 'AccumulatedDepreciation' (減価償却累計額"
    , asDescription = "Liability: Reserve for depreciation (減価償却引当金 — SNA\\/マクロ系の旧称。簿記の間接法には 'AccumulatedDepreciation' (減価償却累計額) を使う)"
    , asAliases = ["減価償却引当金"]
    }
accountSpec DepositPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Deposits accepted"
    , asNameJa = "受入預金 — 銀行側の負債としての預金。SNA\\/マクロ系。従業員等からの預り金は 'DepositsReceived'"
    , asDescription = "Liability: Deposits accepted (受入預金 — 銀行側の負債としての預金。SNA\\/マクロ系。従業員等からの預り金は 'DepositsReceived')"
    , asAliases = ["受入預金"]
    }
accountSpec LongTermNationalBondsPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Long-term national bonds payable"
    , asNameJa = "長期国債, 発行側"
    , asDescription = "Liability: Long-term national bonds payable (長期国債, 発行側)"
    , asAliases = ["長期国債"]
    }
accountSpec ShortTermNationalBondsPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Short-term national bonds payable"
    , asNameJa = "短期国債, 発行側"
    , asDescription = "Liability: Short-term national bonds payable (短期国債, 発行側)"
    , asAliases = ["短期国債"]
    }
accountSpec ReserveDepositPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Reserve deposits, liability side"
    , asNameJa = "準備預金, 負債側 — 中央銀行が受け入れる準備預金。SNA\\/マクロ系"
    , asDescription = "Liability: Reserve deposits, liability side (準備預金, 負債側 — 中央銀行が受け入れる準備預金。SNA\\/マクロ系)。※簿記の買掛金は 'AccountsPayable' を使うこと (旧 examples が本科目を買掛金の代用にしていた経緯あり)"
    , asAliases = ["準備預金"]
    }
accountSpec CentralBankNotePayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Central bank notes"
    , asNameJa = "発行銀行券。SNA\\/マクロ系"
    , asDescription = "Liability: Central bank notes (発行銀行券。SNA\\/マクロ系)"
    , asAliases = ["発行銀行券"]
    }
accountSpec Depreciation = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Depreciation"
    , asNameJa = "減価償却費"
    , asDescription = "Expense: Depreciation (減価償却費)"
    , asAliases = ["減価償却費"]
    }
accountSpec AmortizationExpense = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Amortization expense for intangibles"
    , asNameJa = "無形固定資産償却費 — 特許権・商標権・ソフトウェア等の無形資産の償却費。有形の 'Depreciation' (減価償却費"
    , asDescription = "Expense: Amortization expense for intangibles (無形固定資産償却費 — 特許権・商標権・ソフトウェア等の無形資産の償却費。有形の 'Depreciation' (減価償却費) と区別される)"
    , asAliases = []
    }
accountSpec SalesCost = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Cost of sales"
    , asNameJa = "売上原価"
    , asDescription = "Expense: Cost of sales (売上原価)"
    , asAliases = ["売上原価", "cogs", "cost of sales"]
    }
accountSpec BusinessTrip = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Travel and transportation"
    , asNameJa = "旅費交通費"
    , asDescription = "Expense: Travel and transportation (旅費交通費)"
    , asAliases = ["旅費交通費"]
    }
accountSpec Commutation = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Communication"
    , asNameJa = "通信費 — 旧称。新規コードでは 'CommunicationExpenses' を使う"
    , asDescription = "Expense: Communication (通信費 — 旧称。新規コードでは 'CommunicationExpenses' を使う)"
    , asAliases = ["通信費"]
    }
accountSpec UtilitiesExpense = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Utilities"
    , asNameJa = "水道光熱費"
    , asDescription = "Expense: Utilities (水道光熱費)"
    , asAliases = ["水道光熱費"]
    }
accountSpec RentExpense = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Rent"
    , asNameJa = "支払家賃"
    , asDescription = "Expense: Rent (支払家賃)"
    , asAliases = ["支払家賃"]
    }
accountSpec AdvertisingExpense = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Advertising"
    , asNameJa = "広告宣伝費"
    , asDescription = "Expense: Advertising (広告宣伝費)"
    , asAliases = ["広告宣伝費"]
    }
accountSpec DeliveryExpenses = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Delivery"
    , asNameJa = "発送費"
    , asDescription = "Expense: Delivery (発送費)"
    , asAliases = ["発送費"]
    }
accountSpec SuppliesExpenses = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Supplies"
    , asNameJa = "消耗品費"
    , asDescription = "Expense: Supplies (消耗品費)"
    , asAliases = ["消耗品費"]
    }
accountSpec MiscellaneousExpenses = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Miscellaneous"
    , asNameJa = "雑費"
    , asDescription = "Expense: Miscellaneous (雑費)"
    , asAliases = ["雑費"]
    }
accountSpec WageExpenditure = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Wages"
    , asNameJa = "給料"
    , asDescription = "Expense: Wages (給料)"
    , asAliases = ["給料"]
    }
accountSpec InterestExpense = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Interest expense"
    , asNameJa = "支払利息"
    , asDescription = "Expense: Interest expense (支払利息)"
    , asAliases = ["支払利息"]
    }
accountSpec TaxesExpense = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Taxes"
    , asNameJa = "租税公課"
    , asDescription = "Expense: Taxes (租税公課)"
    , asAliases = ["租税公課"]
    }
accountSpec ConsumptionExpenditure = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Consumption expenditure"
    , asNameJa = "消費支出。SNA\\/マクロ系"
    , asDescription = "Expense: Consumption expenditure (消費支出。SNA\\/マクロ系)"
    , asAliases = ["消費支出"]
    }
accountSpec SubsidyExpense = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Subsidy expenditure"
    , asNameJa = "補助金支出。SNA\\/マクロ系"
    , asDescription = "Expense: Subsidy expenditure (補助金支出。SNA\\/マクロ系)"
    , asAliases = ["補助金支出"]
    }
accountSpec CentralBankPaymentExpense = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Central bank payment to treasury"
    , asNameJa = "国庫納付金支出。SNA\\/マクロ系"
    , asDescription = "Expense: Central bank payment to treasury (国庫納付金支出。SNA\\/マクロ系)"
    , asAliases = ["国庫納付金支出"]
    }
accountSpec Purchases = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Purchases"
    , asNameJa = "仕入"
    , asDescription = "Expense: Purchases (仕入)"
    , asAliases = ["仕入"]
    }
accountSpec NetIncome = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Net income"
    , asNameJa = "当期純利益 — 決算振替用。借方側に立つため Expense 区分"
    , asDescription = "Expense: Net income (当期純利益 — 決算振替用。借方側に立つため Expense 区分)"
    , asAliases = ["当期純利益"]
    }
accountSpec ValueAdded = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Value added"
    , asNameJa = "付加価値。SNA\\/マクロ系"
    , asDescription = "Revenue: Value added (付加価値。SNA\\/マクロ系)"
    , asAliases = ["付加価値"]
    }
accountSpec SubsidyIncome = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Subsidy income"
    , asNameJa = "補助金収入。SNA\\/マクロ系"
    , asDescription = "Revenue: Subsidy income (補助金収入。SNA\\/マクロ系)"
    , asAliases = ["補助金収入"]
    }
accountSpec NationalBondInterestEarned = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "National bond interest earned"
    , asNameJa = "国債利息収入"
    , asDescription = "Revenue: National bond interest earned (国債利息収入)"
    , asAliases = ["国債利息収入"]
    }
accountSpec DepositInterestEarned = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Deposit interest earned"
    , asNameJa = "預金利息収入"
    , asDescription = "Revenue: Deposit interest earned (預金利息収入)"
    , asAliases = ["預金利息収入"]
    }
accountSpec GrossProfit = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gross profit"
    , asNameJa = "売上総利益 — 決算振替用"
    , asDescription = "Revenue: Gross profit (売上総利益 — 決算振替用)"
    , asAliases = ["売上総利益"]
    }
accountSpec OrdinaryProfit = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Ordinary profit"
    , asNameJa = "経常利益 — 決算振替用"
    , asDescription = "Revenue: Ordinary profit (経常利益 — 決算振替用)"
    , asAliases = ["経常利益"]
    }
accountSpec InterestEarned = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Interest earned"
    , asNameJa = "受取利息"
    , asDescription = "Revenue: Interest earned (受取利息)"
    , asAliases = ["受取利息"]
    }
accountSpec ReceiptFee = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Receipt fee"
    , asNameJa = "受取手数料。支払側は 'PaymentFees'"
    , asDescription = "Revenue: Receipt fee (受取手数料。支払側は 'PaymentFees')"
    , asAliases = ["受取手数料"]
    }
accountSpec RentalIncome = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Rental income"
    , asNameJa = "受取家賃"
    , asDescription = "Revenue: Rental income (受取家賃)"
    , asAliases = ["受取家賃"]
    }
accountSpec WageEarned = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Wage income"
    , asNameJa = "賃金収入。SNA\\/マクロ系"
    , asDescription = "Revenue: Wage income (賃金収入。SNA\\/マクロ系)"
    , asAliases = ["賃金収入"]
    }
accountSpec TaxesRevenue = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Tax revenue"
    , asNameJa = "租税収入。SNA\\/マクロ系"
    , asDescription = "Revenue: Tax revenue (租税収入。SNA\\/マクロ系)"
    , asAliases = ["租税収入"]
    }
accountSpec CentralBankPaymentIncome = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Central bank payment to treasury"
    , asNameJa = "国庫納付金収入。SNA\\/マクロ系"
    , asDescription = "Revenue: Central bank payment to treasury (国庫納付金収入。SNA\\/マクロ系)"
    , asAliases = ["国庫納付金収入"]
    }
accountSpec Sales = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Sales"
    , asNameJa = "売上"
    , asDescription = "Revenue: Sales (売上)"
    , asAliases = ["売上"]
    }
accountSpec EquityInEarningsOfInvestee = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Equity in earnings of investee"
    , asNameJa = "持分法による投資利益"
    , asDescription = "Revenue: Equity in earnings of investee (持分法による投資利益). Recognised under the equity method."
    , asAliases = []
    }
accountSpec NetLoss = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Net loss"
    , asNameJa = "当期純損失 — 決算振替用。貸方側に立つため Revenue 区分"
    , asDescription = "Revenue: Net loss (当期純損失 — 決算振替用。貸方側に立つため Revenue 区分)"
    , asAliases = ["当期純損失"]
    }
accountSpec PettyCash = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Petty cash"
    , asNameJa = "小口現金"
    , asDescription = "Asset: Petty cash (小口現金)"
    , asAliases = ["小口現金"]
    }
accountSpec NotesReceivable = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Notes receivable"
    , asNameJa = "受取手形"
    , asDescription = "Asset: Notes receivable (受取手形)"
    , asAliases = ["受取手形"]
    }
accountSpec ElectronicallyRecordedReceivable = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Electronically recorded monetary claims"
    , asNameJa = "電子記録債権"
    , asDescription = "Asset: Electronically recorded monetary claims (電子記録債権)"
    , asAliases = ["電子記録債権"]
    }
accountSpec CreditCardReceivable = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Credit card receivable"
    , asNameJa = "クレジット売掛金"
    , asDescription = "Asset: Credit card receivable (クレジット売掛金)"
    , asAliases = ["クレジット売掛金"]
    }
accountSpec NotesLoansReceivable = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Loans receivable on notes"
    , asNameJa = "手形貸付金"
    , asDescription = "Asset: Loans receivable on notes (手形貸付金)"
    , asAliases = ["手形貸付金"]
    }
accountSpec MerchandiseInventory = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Merchandise inventory"
    , asNameJa = "繰越商品"
    , asDescription = "Asset: Merchandise inventory (繰越商品). Use under the periodic/3-account method (3 分法: Purchases\\/Sales\\/MerchandiseInventory). For the perpetual\\/specific-identification method (分記法) use 'Products' instead."
    , asAliases = ["繰越商品"]
    }
accountSpec AdvancesPaid = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Advances paid"
    , asNameJa = "前払金"
    , asDescription = "Asset: Advances paid (前払金)"
    , asAliases = ["前払金"]
    }
accountSpec PrepaidExpenses = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Prepaid expenses"
    , asNameJa = "前払費用"
    , asDescription = "Asset: Prepaid expenses (前払費用), deferral accrual account (経過勘定)"
    , asAliases = ["前払費用"]
    }
accountSpec AccruedRevenue = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued revenue"
    , asNameJa = "未収収益"
    , asDescription = "Asset: Accrued revenue (未収収益), deferral accrual account (経過勘定)"
    , asAliases = ["未収収益"]
    }
accountSpec OtherReceivables = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Other receivables"
    , asNameJa = "未収入金"
    , asDescription = "Asset: Other receivables (未収入金)"
    , asAliases = ["未収入金"]
    }
accountSpec PaymentsOnBehalf = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Payments made on behalf"
    , asNameJa = "立替金"
    , asDescription = "Asset: Payments made on behalf (立替金)"
    , asAliases = ["立替金"]
    }
accountSpec SuspensePayments = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Suspense payments"
    , asNameJa = "仮払金"
    , asDescription = "Asset: Suspense payments (仮払金)"
    , asAliases = ["仮払金"]
    }
accountSpec ConsumptionTaxPaid = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Consumption tax paid"
    , asNameJa = "仮払消費税"
    , asDescription = "Asset: Consumption tax paid (仮払消費税)"
    , asAliases = ["仮払消費税"]
    }
accountSpec PrepaidCorporateIncomeTaxes = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Prepaid corporate income taxes"
    , asNameJa = "仮払法人税等"
    , asDescription = "Asset: Prepaid corporate income taxes (仮払法人税等)"
    , asAliases = ["仮払法人税等"]
    }
accountSpec Land = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Land"
    , asNameJa = "土地"
    , asDescription = "Asset: Land (土地)"
    , asAliases = ["土地"]
    }
accountSpec Fixtures = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Fixtures and equipment"
    , asNameJa = "備品"
    , asDescription = "Asset: Fixtures and equipment (備品)"
    , asAliases = ["備品"]
    }
accountSpec Patent = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Patent"
    , asNameJa = "特許権"
    , asDescription = "Asset: Patent (特許権)"
    , asAliases = ["特許権"]
    }
accountSpec Trademark = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Trademark"
    , asNameJa = "商標権"
    , asDescription = "Asset: Trademark (商標権)"
    , asAliases = ["商標権"]
    }
accountSpec Software = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Software"
    , asNameJa = "ソフトウェア"
    , asDescription = "Asset: Software (ソフトウェア)"
    , asAliases = ["ソフトウェア"]
    }
accountSpec CashOverShort = Just AccountSpec
    { asDivision = Assets
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Cash over and short"
    , asNameJa = "現金過不足"
    , asDescription = "Asset: Cash over and short (現金過不足), a temporary/suspense account cleared at closing to MiscellaneousIncome\\/MiscellaneousLoss"
    , asAliases = ["現金過不足"]
    }
accountSpec AccountsPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accounts payable"
    , asNameJa = "買掛金"
    , asDescription = "Liability: Accounts payable (買掛金)"
    , asAliases = ["買掛金", "a/p", "accounts payable"]
    }
accountSpec NotesPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Notes payable"
    , asNameJa = "支払手形"
    , asDescription = "Liability: Notes payable (支払手形)"
    , asAliases = ["支払手形"]
    }
accountSpec ElectronicallyRecordedObligations = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Electronically recorded monetary obligations"
    , asNameJa = "電子記録債務"
    , asDescription = "Liability: Electronically recorded monetary obligations (電子記録債務)"
    , asAliases = ["電子記録債務"]
    }
accountSpec NotesLoansPayable = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Loans payable on notes"
    , asNameJa = "手形借入金"
    , asDescription = "Liability: Loans payable on notes (手形借入金)"
    , asAliases = ["手形借入金"]
    }
accountSpec BankOverdraft = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Bank overdraft"
    , asNameJa = "当座借越"
    , asDescription = "Liability: Bank overdraft (当座借越)"
    , asAliases = ["当座借越"]
    }
accountSpec AdvancesReceived = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Advances received"
    , asNameJa = "前受金"
    , asDescription = "Liability: Advances received (前受金)"
    , asAliases = ["前受金"]
    }
accountSpec UnearnedRevenue = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Unearned revenue"
    , asNameJa = "前受収益"
    , asDescription = "Liability: Unearned revenue (前受収益), deferral accrual account (経過勘定)"
    , asAliases = ["前受収益"]
    }
accountSpec AccruedExpenses = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued expenses"
    , asNameJa = "未払費用"
    , asDescription = "Liability: Accrued expenses (未払費用), deferral accrual account (経過勘定)"
    , asAliases = ["未払費用"]
    }
accountSpec OtherPayables = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Other payables"
    , asNameJa = "未払金"
    , asDescription = "Liability: Other payables (未払金)"
    , asAliases = ["未払金"]
    }
accountSpec DepositsReceived = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Deposits received"
    , asNameJa = "預り金"
    , asDescription = "Liability: Deposits received (預り金)"
    , asAliases = ["預り金"]
    }
accountSpec SuspenseReceipts = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Suspense receipts"
    , asNameJa = "仮受金"
    , asDescription = "Liability: Suspense receipts (仮受金)"
    , asAliases = ["仮受金"]
    }
accountSpec ConsumptionTaxReceived = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Consumption tax received"
    , asNameJa = "仮受消費税"
    , asDescription = "Liability: Consumption tax received (仮受消費税)"
    , asAliases = ["仮受消費税"]
    }
accountSpec AccruedConsumptionTax = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued (unpaid) consumption tax"
    , asNameJa = "未払消費税"
    , asDescription = "Liability: Accrued (unpaid) consumption tax (未払消費税)"
    , asAliases = ["未払消費税"]
    }
accountSpec AccruedCorporateIncomeTaxes = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Accrued (unpaid) corporate income taxes"
    , asNameJa = "未払法人税等"
    , asDescription = "Liability: Accrued (unpaid) corporate income taxes (未払法人税等)"
    , asAliases = ["未払法人税等"]
    }
accountSpec UnpaidDividends = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Unpaid dividends"
    , asNameJa = "未払配当金"
    , asDescription = "Liability: Unpaid dividends (未払配当金)"
    , asAliases = ["未払配当金"]
    }
accountSpec AllowanceForDoubtfulAccounts = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Current
    , asNameEn = "Allowance for doubtful accounts"
    , asNameJa = "貸倒引当金"
    , asDescription = "Liability: Allowance for doubtful accounts (貸倒引当金). Modeled as an independent credit-balance valuation account (評価勘定) classified under Liability: this keeps values non-negative and the Hat\\/Not structure intact, deferring the B\\/S contra-asset (deduction) presentation to the Write side, rather than adding a new AccountDivision (over-engineered at the 3-級 level)."
    , asAliases = ["貸倒引当金"]
    }
accountSpec AccumulatedDepreciation = Just AccountSpec
    { asDivision = Liability
    , asIsContra = False
    , asFixedCurrent = Fixed
    , asNameEn = "Accumulated depreciation"
    , asNameJa = "減価償却累計額"
    , asDescription = "Liability: Accumulated depreciation (減価償却累計額), valuation account (評価勘定) under the indirect method (間接法). Classified under Liability for the same reason as 'AllowanceForDoubtfulAccounts'. This is the canonical bookkeeping account for accumulated depreciation; the existing 'ReserveForDepreciation' is retained as the legacy SNA\\/macro-accounting name."
    , asAliases = ["減価償却累計額"]
    }
accountSpec LegalRetainedEarnings = Just AccountSpec
    { asDivision = Equity
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Legal (appropriated) retained earnings reserve"
    , asNameJa = "利益準備金"
    , asDescription = "Equity: Legal (appropriated) retained earnings reserve (利益準備金)"
    , asAliases = ["利益準備金"]
    }
accountSpec CumulativeTranslationAdjustment = Just AccountSpec
    { asDivision = Equity
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Cumulative translation adjustment"
    , asNameJa = "為替換算調整勘定; 在外子会社等の外貨建財務諸表の換算差額を計上する OCI/資本の部の項目"
    , asDescription = "Equity: Cumulative translation adjustment (為替換算調整勘定; 在外子会社等の外貨建財務諸表の換算差額を計上する OCI/資本の部の項目)"
    , asAliases = []
    }
accountSpec ProvisionForDoubtfulAccounts = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Provision for doubtful accounts"
    , asNameJa = "貸倒引当金繰入"
    , asDescription = "Cost: Provision for doubtful accounts (貸倒引当金繰入)"
    , asAliases = ["貸倒引当金繰入"]
    }
accountSpec BadDebtLoss = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Bad debt loss"
    , asNameJa = "貸倒損失"
    , asDescription = "Cost: Bad debt loss (貸倒損失)"
    , asAliases = ["貸倒損失"]
    }
accountSpec LossOnSalesOfFixedAssets = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on sales of fixed assets"
    , asNameJa = "固定資産売却損"
    , asDescription = "Cost: Loss on sales of fixed assets (固定資産売却損)"
    , asAliases = ["固定資産売却損"]
    }
accountSpec LossOnSalesOfNotesReceivable = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Loss on sales of notes receivable"
    , asNameJa = "手形売却損"
    , asDescription = "Cost: Loss on sales of notes receivable (手形売却損)"
    , asAliases = ["手形売却損"]
    }
accountSpec PaymentFees = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Payment fees / fees paid"
    , asNameJa = "支払手数料"
    , asDescription = "Cost: Payment fees / fees paid (支払手数料), the debit counterpart of 'ReceiptFee'"
    , asAliases = ["支払手数料"]
    }
accountSpec MiscellaneousLoss = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Miscellaneous loss"
    , asNameJa = "雑損"
    , asDescription = "Cost: Miscellaneous loss (雑損)"
    , asAliases = ["雑損"]
    }
accountSpec CorporateIncomeTaxes = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Corporate income taxes"
    , asNameJa = "法人税等"
    , asDescription = "Cost: Corporate income taxes (法人税等)"
    , asAliases = ["法人税等"]
    }
accountSpec CommunicationExpenses = Just AccountSpec
    { asDivision = Cost
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Communication expenses"
    , asNameJa = "通信費"
    , asDescription = "Cost: Communication expenses (通信費). Explicitly named counterpart of the legacy 'Commutation' (also \"Communication\"); 'Commutation' is retained for backward compatibility."
    , asAliases = ["通信費"]
    }
accountSpec GainOnSalesOfFixedAssets = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Gain on sales of fixed assets"
    , asNameJa = "固定資産売却益"
    , asDescription = "Revenue: Gain on sales of fixed assets (固定資産売却益)"
    , asAliases = ["固定資産売却益"]
    }
accountSpec RecoveryOfBadDebts = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Recovery of bad debts written off"
    , asNameJa = "償却債権取立益"
    , asDescription = "Revenue: Recovery of bad debts written off (償却債権取立益)"
    , asAliases = ["償却債権取立益"]
    }
accountSpec MiscellaneousIncome = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Miscellaneous income"
    , asNameJa = "雑益"
    , asDescription = "Revenue: Miscellaneous income (雑益)"
    , asAliases = ["雑益"]
    }
accountSpec ReversalOfAllowanceForDoubtfulAccounts = Just AccountSpec
    { asDivision = Revenue
    , asIsContra = False
    , asFixedCurrent = Other
    , asNameEn = "Reversal of allowance for doubtful accounts"
    , asNameJa = "貸倒引当金戻入"
    , asDescription = "Revenue: Reversal of allowance for doubtful accounts (貸倒引当金戻入). Credit counterpart used by the 差額補充法/洗替法 when the estimated allowance is smaller than the existing balance (the excess of 'AllowanceForDoubtfulAccounts' is released)."
    , asAliases = ["貸倒引当金戻入"]
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
-- Land 1 preserves the former semantics, so every title returns False,
-- including the wildcard. Complexity: O(1)
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
