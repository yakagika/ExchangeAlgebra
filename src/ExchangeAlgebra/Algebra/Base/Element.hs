{- |
    Module     : ExchangeAlgebra.Algebra.Base.Element
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    == Extending with user-defined types

    To use your own type as a basis component, declare an 'Element' instance.
    A single distinguished value must serve as the wildcard used by the
    transfer engine and by projection operations:

    @
    data Company = CompanyA | CompanyB | CompanyWildcard
      deriving (Eq, Ord, Show, Generic, Hashable, Typeable)

    instance Element Company where
      wildcard = CompanyWildcard
    @

    == Import guidance

    User code rarely needs to import this module directly. The 'Element'
    class and its associated symbols are re-exported from
    "ExchangeAlgebra.Algebra.Base", and transitively from
    "ExchangeAlgebra.Algebra", "ExchangeAlgebra.Journal", and the top-level
    "ExchangeAlgebra". The module path of this file may be reorganized in
    future versions; prefer importing from the higher-level modules.

-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE DeriveGeneric              #-}

-- NB. Only the two names a user needs to define an 'Element' instance are
-- re-exported ('Hashable' for the constraint/instance, 'Generic' for deriving).
-- The former whole-module re-exports of "Data.Hashable" and "GHC.Generics"
-- leaked their entire namespaces (hash, hashUsing, Rep, from/to, …) through
-- Base -> Algebra -> the ExchangeAlgebra umbrella.
module ExchangeAlgebra.Algebra.Base.Element
    ( module ExchangeAlgebra.Algebra.Base.Element
    , Hashable(..)
    , Generic) where

import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.Time           as Time
import              Data.Time
import GHC.Generics (Generic)
import Data.Hashable
import Data.Typeable (Typeable, cast, typeOf)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as BinaryGet
import qualified Data.Binary.Put as BinaryPut

------------------------------------------------------------------
-- * Element (components of bases)
------------------------------------------------------------------

-- | Element Class: a type must be an instance of this class to serve as a component of a basis.
--
-- Each component of a basis must be an instance of this type class.
-- It provides wildcard-based pattern matching, enabling flexible basis
-- specification in transfer transformations and projections.
class (Eq a, Ord a, Show a, Hashable a, Typeable a) => Element a where

    -- | The wildcard value. Used for pattern matching in search, transfer transformation, etc.
    --
    -- Complexity: O(1)
    wildcard       :: a

    -- | Determines whether the element itself or any of its internal components contains a wildcard.
    -- For tuple elements, returns True if any component is a wildcard.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE haveWildcard #-}
    haveWildcard :: a -> Bool
    haveWildcard = isWildcard

    -- | Determines whether the value is exactly the wildcard.
    --
    -- Complexity: O(1)
    {-# INLINE isWildcard #-}
    isWildcard     :: a -> Bool
    isWildcard a = a == wildcard

    -- | Wildcard-ignoring transformation.
    -- If @after@ is a wildcard, returns @before@.
    -- Used inside transfer to fill wildcard positions in the target basis with the original values.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE ignoreWildcard #-}
    ignoreWildcard :: a -> a -> a
    ignoreWildcard before after
        | before == after   = before
        | isWildcard after = before
        | otherwise         = after

    -- | Wildcard-aware equality test.
    -- Returns True if either operand is a wildcard.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE equal #-}
    equal :: a -> a -> Bool
    equal a b | isWildcard a = True
              | isWildcard b = True
              | otherwise     = a == b

    -- | Equality operator that treats wildcards as equal.
    -- Unlike '==', @(.==)@ matches tuples that partially contain wildcards.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE (.==)  #-}
    (.==) :: a -> a -> Bool
    (.==) a b = a == b || (haveWildcard a || haveWildcard b) && equal a b

    -- | Inequality operator that treats wildcards as equal. Negation of @(.==)@.
    --
    -- Complexity: O(k)
    {-# INLINE (./=) #-}
    (./=) :: a -> a -> Bool
    (./=) a b = not (a .== b)

    -- | Wildcard-aware comparison.
    -- Returns EQ if the two values are equal under @(.==)@.
    --
    -- Complexity: O(k)
    {-# INLINE compareElement #-}
    compareElement :: a -> a -> Ordering
    compareElement x y
        | x .== y = EQ
        | otherwise = compare x y

    -- | Wildcard-aware less-than comparison.
    --
    -- Complexity: O(k)
    (.<) :: a -> a -> Bool
    (.<) x y = compareElement x y == LT

    -- | Wildcard-aware greater-than comparison.
    --
    -- Complexity: O(k)
    (.>) :: a -> a -> Bool
    (.>) x y = compareElement x y == GT

    -- | Wildcard-aware less-than-or-equal comparison.
    --
    -- Complexity: O(k)
    (.<=) :: a -> a -> Bool
    (.<=) x y = compareElement x y /= GT

    -- | Wildcard-aware greater-than-or-equal comparison.
    --
    -- Complexity: O(k)
    (.>=) :: a -> a -> Bool
    (.>=) x y = compareElement x y /= LT

    -- | Wildcard-aware maximum.
    --
    -- Complexity: O(k)
    maxElement :: a -> a -> a
    maxElement x y
        | x .>= y = x
        | otherwise = y

    -- | Wildcard-aware minimum.
    --
    -- Complexity: O(k)
    minElement :: a -> a -> a
    minElement x y
        | x .<= y = x
        | otherwise = y

-- | An existential type that holds each axis of a basis with its type erased.
-- Used to decompose multi-dimensional bases (tuples) into per-axis keys for indexing.
data AxisKey = forall a. Element a => AxisKey !a

instance Eq AxisKey where
    AxisKey x == AxisKey y = case cast y of
        Nothing -> False
        Just y' -> x == y'

instance Hashable AxisKey where
    hashWithSalt salt (AxisKey x) = salt `hashWithSalt` (typeOf x) `hashWithSalt` x

{-# INLINE axisIsWildcard #-}
axisIsWildcard :: AxisKey -> Bool
axisIsWildcard (AxisKey x) = isWildcard x

-- | A type class for decomposing a basis element into a list of per-axis t'AxisKey's.
-- Overlapping instances are defined for tuple types so that each component
-- is decomposed into a separate t'AxisKey'.
--
-- Complexity: O(k) (k is the number of tuple components)
class (Element a) => AxisDecompose a where
    toAxisKeys :: a -> [AxisKey]

instance {-# OVERLAPPABLE #-} Element a => AxisDecompose a where
    {-# INLINE toAxisKeys #-}
    toAxisKeys a = [AxisKey a]

-- | Shorthand notation for the wildcard. An alias for @wildcard@.
-- Write @(.#)@ when specifying patterns in projections and transfer transformations.
--
-- Complexity: O(1)
{-# INLINE (.#) #-}
(.#) :: Element a => a
(.#) = wildcard

infix 4 .==
infix 4 ./=
------------------------------------------------------------------
-- * Elements
------------------------------------------------------------------

-- ** Account Titles

data  AccountTitles = Cash                            -- ^ Asset: Cash (現金)
                    | Deposits                        -- ^ Asset: Savings deposits (普通預金)
                    | CurrentDeposits                 -- ^ Asset: Current deposits (当座預金)
                    | Securities                      -- ^ Asset: Securities (有価証券)
                    | InvestmentSecurities            -- ^ Asset: Investment securities (投資有価証券)
                    | InvestmentInAssociate           -- ^ Asset: Investment in associate (関係会社株式). Carrying amount under the equity method (持分法適用投資勘定).
                    | LongTermNationalBonds           -- ^ Asset: Long-term national bonds (長期国債)
                    | ShortTermNationalBonds          -- ^ Asset: Short-term national bonds (短期国債)
                    | Products                        -- ^ Asset: Products (商品)。分記法用。3 分法 (仕入\/売上\/繰越商品) では 'MerchandiseInventory' を使う
                    | Machinery                       -- ^ Asset: Machinery and equipment (機械装置)
                    | Building                        -- ^ Asset: Real estate (建物)
                    | Vehicle                         -- ^ Asset: Vehicles (車両運搬具)
                    | StockInvestment                 -- ^ Asset: Stock investment (株式投資)
                    | EquipmentInvestment             -- ^ Asset: Equipment investment (設備投資)
                    | LongTermLoansReceivable         -- ^ Asset: Long-term loans receivable (長期貸付金)
                    | AccountsReceivable              -- ^ Asset: Accounts receivable (売掛金)
                    | ShortTermLoansReceivable        -- ^ Asset: Short-term loans receivable (短期貸付金)
                    | ReserveDepositReceivable        -- ^ Asset: Reserve deposits, asset side (準備預金, 資産側 — 市中銀行が中央銀行に置く準備預金。SNA\/マクロ系)
                    | Gold                            -- ^ Asset: Gold (金)
                    | GovernmentService               -- ^ Asset: Government service (政府サービス。SNA\/マクロ系)
                    | CapitalStock                    -- ^ Equity: Capital stock (資本金)
                    | RetainedEarnings                -- ^ Equity: Retained earnings (繰越利益剰余金)
                    | LongTermLoansPayable            -- ^ Liability: Long-term loans payable (長期借入金)
                    | ShortTermLoansPayable           -- ^ Liability: Short-term loans payable (短期借入金)
                    | LoansPayable                    -- ^ Liability: Loans payable (借入金)
                    | ReserveForDepreciation          -- ^ Liability: Reserve for depreciation (減価償却引当金 — SNA\/マクロ系の旧称。簿記の間接法には 'AccumulatedDepreciation' (減価償却累計額) を使う)
                    | DepositPayable                  -- ^ Liability: Deposits accepted (受入預金 — 銀行側の負債としての預金。SNA\/マクロ系。従業員等からの預り金は 'DepositsReceived')
                    | LongTermNationalBondsPayable    -- ^ Liability: Long-term national bonds payable (長期国債, 発行側)
                    | ShortTermNationalBondsPayable   -- ^ Liability: Short-term national bonds payable (短期国債, 発行側)
                    | ReserveDepositPayable           -- ^ Liability: Reserve deposits, liability side (準備預金, 負債側 — 中央銀行が受け入れる準備預金。SNA\/マクロ系)。※簿記の買掛金は 'AccountsPayable' を使うこと (旧 examples が本科目を買掛金の代用にしていた経緯あり)
                    | CentralBankNotePayable          -- ^ Liability: Central bank notes (発行銀行券。SNA\/マクロ系)
                    | Depreciation                    -- ^ Expense: Depreciation (減価償却費)
                    | AmortizationExpense             -- ^ Expense: Amortization expense for intangibles (無形固定資産償却費 — 特許権・商標権・ソフトウェア等の無形資産の償却費。有形の 'Depreciation' (減価償却費) と区別される)
                    | SalesCost                       -- ^ Expense: Cost of sales (売上原価)
                    | BusinessTrip                    -- ^ Expense: Travel and transportation (旅費交通費)
                    | Commutation                     -- ^ Expense: Communication (通信費 — 旧称。新規コードでは 'CommunicationExpenses' を使う)
                    | UtilitiesExpense                -- ^ Expense: Utilities (水道光熱費)
                    | RentExpense                     -- ^ Expense: Rent (支払家賃)
                    | AdvertisingExpense              -- ^ Expense: Advertising (広告宣伝費)
                    | DeliveryExpenses                -- ^ Expense: Delivery (発送費)
                    | SuppliesExpenses                -- ^ Expense: Supplies (消耗品費)
                    | MiscellaneousExpenses           -- ^ Expense: Miscellaneous (雑費)
                    | WageExpenditure                 -- ^ Expense: Wages (給料)
                    | InterestExpense                 -- ^ Expense: Interest expense (支払利息)
                    | TaxesExpense                    -- ^ Expense: Taxes (租税公課)
                    | ConsumptionExpenditure          -- ^ Expense: Consumption expenditure (消費支出。SNA\/マクロ系)
                    | SubsidyExpense                  -- ^ Expense: Subsidy expenditure (補助金支出。SNA\/マクロ系)
                    | CentralBankPaymentExpense       -- ^ Expense: Central bank payment to treasury (国庫納付金支出。SNA\/マクロ系)
                    | Purchases                       -- ^ Expense: Purchases (仕入)
                    | NetIncome                       -- ^ Expense: Net income (当期純利益 — 決算振替用。借方側に立つため Expense 区分)
                    | ValueAdded                      -- ^ Revenue: Value added (付加価値。SNA\/マクロ系)
                    | SubsidyIncome                   -- ^ Revenue: Subsidy income (補助金収入。SNA\/マクロ系)
                    | NationalBondInterestEarned      -- ^ Revenue: National bond interest earned (国債利息収入)
                    | DepositInterestEarned           -- ^ Revenue: Deposit interest earned (預金利息収入)
                    | GrossProfit                     -- ^ Revenue: Gross profit (売上総利益 — 決算振替用)
                    | OrdinaryProfit                  -- ^ Revenue: Ordinary profit (経常利益 — 決算振替用)
                    | InterestEarned                  -- ^ Revenue: Interest earned (受取利息)
                    | ReceiptFee                      -- ^ Revenue: Receipt fee (受取手数料。支払側は 'PaymentFees')
                    | RentalIncome                    -- ^ Revenue: Rental income (受取家賃)
                    | WageEarned                      -- ^ Revenue: Wage income (賃金収入。SNA\/マクロ系)
                    | TaxesRevenue                    -- ^ Revenue: Tax revenue (租税収入。SNA\/マクロ系)
                    | CentralBankPaymentIncome        -- ^ Revenue: Central bank payment to treasury (国庫納付金収入。SNA\/マクロ系)
                    | Sales                           -- ^ Revenue: Sales (売上)
                    | EquityInEarningsOfInvestee      -- ^ Revenue: Equity in earnings of investee (持分法による投資利益). Recognised under the equity method.
                    | NetLoss                         -- ^ Revenue: Net loss (当期純損失 — 決算振替用。貸方側に立つため Revenue 区分)
                    -- Elementary bookkeeping (日商簿記 3 級水準) additions.
                    -- Appended before the 'AccountTitle' wildcard so that only the
                    -- wildcard's Enum/Binary ordinal shifts (existing serialized
                    -- bases remain compatible).
                    --
                    -- Assets (資産)
                    | PettyCash                       -- ^ Asset: Petty cash (小口現金)
                    | NotesReceivable                 -- ^ Asset: Notes receivable (受取手形)
                    | ElectronicallyRecordedReceivable -- ^ Asset: Electronically recorded monetary claims (電子記録債権)
                    | CreditCardReceivable            -- ^ Asset: Credit card receivable (クレジット売掛金)
                    | NotesLoansReceivable            -- ^ Asset: Loans receivable on notes (手形貸付金)
                    | MerchandiseInventory            -- ^ Asset: Merchandise inventory (繰越商品). Use under the periodic/3-account method (3 分法: Purchases\/Sales\/MerchandiseInventory). For the perpetual\/specific-identification method (分記法) use 'Products' instead.
                    | AdvancesPaid                    -- ^ Asset: Advances paid (前払金)
                    | PrepaidExpenses                 -- ^ Asset: Prepaid expenses (前払費用), deferral accrual account (経過勘定)
                    | AccruedRevenue                  -- ^ Asset: Accrued revenue (未収収益), deferral accrual account (経過勘定)
                    | OtherReceivables                -- ^ Asset: Other receivables (未収入金)
                    | PaymentsOnBehalf                -- ^ Asset: Payments made on behalf (立替金)
                    | SuspensePayments                -- ^ Asset: Suspense payments (仮払金)
                    | ConsumptionTaxPaid              -- ^ Asset: Consumption tax paid (仮払消費税)
                    | PrepaidCorporateIncomeTaxes     -- ^ Asset: Prepaid corporate income taxes (仮払法人税等)
                    | Land                            -- ^ Asset: Land (土地)
                    | Fixtures                        -- ^ Asset: Fixtures and equipment (備品)
                    | Patent                          -- ^ Asset: Patent (特許権)
                    | Trademark                       -- ^ Asset: Trademark (商標権)
                    | Software                        -- ^ Asset: Software (ソフトウェア)
                    | CashOverShort                   -- ^ Asset: Cash over and short (現金過不足), a temporary/suspense account cleared at closing to MiscellaneousIncome\/MiscellaneousLoss
                    -- Liability (負債)
                    | AccountsPayable                 -- ^ Liability: Accounts payable (買掛金)
                    | NotesPayable                    -- ^ Liability: Notes payable (支払手形)
                    | ElectronicallyRecordedObligations -- ^ Liability: Electronically recorded monetary obligations (電子記録債務)
                    | NotesLoansPayable               -- ^ Liability: Loans payable on notes (手形借入金)
                    | BankOverdraft                   -- ^ Liability: Bank overdraft (当座借越)
                    | AdvancesReceived                -- ^ Liability: Advances received (前受金)
                    | UnearnedRevenue                 -- ^ Liability: Unearned revenue (前受収益), deferral accrual account (経過勘定)
                    | AccruedExpenses                 -- ^ Liability: Accrued expenses (未払費用), deferral accrual account (経過勘定)
                    | OtherPayables                   -- ^ Liability: Other payables (未払金)
                    | DepositsReceived                -- ^ Liability: Deposits received (預り金)
                    | SuspenseReceipts                -- ^ Liability: Suspense receipts (仮受金)
                    | ConsumptionTaxReceived          -- ^ Liability: Consumption tax received (仮受消費税)
                    | AccruedConsumptionTax           -- ^ Liability: Accrued (unpaid) consumption tax (未払消費税)
                    | AccruedCorporateIncomeTaxes     -- ^ Liability: Accrued (unpaid) corporate income taxes (未払法人税等)
                    | UnpaidDividends                 -- ^ Liability: Unpaid dividends (未払配当金)
                    | AllowanceForDoubtfulAccounts    -- ^ Asset (contra): Allowance for doubtful accounts (貸倒引当金), a credit-balance valuation account (評価勘定) deducted from receivables. Division is Assets with @isContra = True@ (Definition 7 amendment, 0.5.0.0); home side stays Credit, values stay non-negative, Hat\/Not structure intact. B\/S deduction (net) presentation is the Write side's job.
                    | AccumulatedDepreciation         -- ^ Asset (contra): Accumulated depreciation (減価償却累計額), valuation account (評価勘定) under the indirect method (間接法), deducted from the related depreciable assets. Division is Assets with @isContra = True@ for the same reason as 'AllowanceForDoubtfulAccounts'. This is the canonical bookkeeping account for accumulated depreciation; the existing 'ReserveForDepreciation' is retained as the legacy SNA\/macro-accounting name.
                    -- Equity (資本)
                    | LegalRetainedEarnings           -- ^ Equity: Legal (appropriated) retained earnings reserve (利益準備金)
                    | CumulativeTranslationAdjustment -- ^ Equity: Cumulative translation adjustment (為替換算調整勘定; 在外子会社等の外貨建財務諸表の換算差額を計上する OCI/資本の部の項目)
                    -- Cost (費用)
                    | ProvisionForDoubtfulAccounts    -- ^ Cost: Provision for doubtful accounts (貸倒引当金繰入)
                    | BadDebtLoss                     -- ^ Cost: Bad debt loss (貸倒損失)
                    | LossOnSalesOfFixedAssets        -- ^ Cost: Loss on sales of fixed assets (固定資産売却損)
                    | LossOnSalesOfNotesReceivable    -- ^ Cost: Loss on sales of notes receivable (手形売却損)
                    | PaymentFees                     -- ^ Cost: Payment fees / fees paid (支払手数料), the debit counterpart of 'ReceiptFee'
                    | MiscellaneousLoss               -- ^ Cost: Miscellaneous loss (雑損)
                    | CorporateIncomeTaxes            -- ^ Cost: Corporate income taxes (法人税等)
                    | CommunicationExpenses           -- ^ Cost: Communication expenses (通信費). Explicitly named counterpart of the legacy 'Commutation' (also "Communication"); 'Commutation' is retained for backward compatibility.
                    -- Revenue (収益)
                    | GainOnSalesOfFixedAssets        -- ^ Revenue: Gain on sales of fixed assets (固定資産売却益)
                    | RecoveryOfBadDebts              -- ^ Revenue: Recovery of bad debts written off (償却債権取立益)
                    | MiscellaneousIncome             -- ^ Revenue: Miscellaneous income (雑益)
                    | ReversalOfAllowanceForDoubtfulAccounts -- ^ Revenue: Reversal of allowance for doubtful accounts (貸倒引当金戻入). Credit counterpart used by the 差額補充法/洗替法 when the estimated allowance is smaller than the existing balance (the excess of 'AllowanceForDoubtfulAccounts' is released).
                    -- V-Land 2: JCCI grade-2 commercial-bookkeeping vocabulary.
                    | TimeDeposits -- ^ Assets: Time deposits (定期預金)
                    | LoansReceivable -- ^ Assets: Loans receivable (貸付金)
                    | GiftCertificatesReceived -- ^ Assets: Gift certificates received (受取商品券)
                    | SecurityDepositsPaid -- ^ Assets: Guarantee deposits (差入保証金)
                    | SuppliesOnHand -- ^ Assets: Supplies (貯蔵品)
                    | ContractAssets -- ^ Assets: Contract assets (契約資産)
                    | IncomeTaxesRefundReceivable -- ^ Assets: Income taxes refund receivable (未収還付法人税等)
                    | WorkInProcess -- ^ Assets: Work in process (仕掛品)
                    | DeferredTaxAssets -- ^ Assets: Deferred tax assets (繰延税金資産)
                    | LeasedAssets -- ^ Assets: Leased assets (リース資産)
                    | ToolsAndInstruments -- ^ Assets: Tools and instruments (工具器具)
                    | ConstructionInProgress -- ^ Assets: Construction in progress (建設仮勘定)
                    | Goodwill -- ^ Assets: Goodwill (のれん)
                    | SoftwareInProgress -- ^ Assets: Software in progress (ソフトウェア仮勘定)
                    | LongTermPrepaidExpenses -- ^ Assets: Long-term prepaid expenses (長期前払費用)
                    | DishonoredNotesReceivable -- ^ Assets: Dishonored notes receivable (不渡手形)
                    | PrepaidPensionCost -- ^ Assets: Prepaid pension costs (前払年金費用)
                    | NetDefinedBenefitAsset -- ^ Assets: Retirement benefit asset (退職給付に係る資産)
                    | DepositsInSpecialAccounts -- ^ Assets: Deposits in special accounts (別段預金)
                    | Structures -- ^ Assets: Structures (構築物)
                    | LeaseholdRights -- ^ Assets: Leasehold interests in land (借地権)
                    | NonOperatingNotesReceivable -- ^ Assets: Non-operating notes receivable (営業外受取手形)
                    | NonOperatingElectronicallyRecordedReceivable -- ^ Assets: Electronically recorded monetary claims - non-operating (営業外電子記録債権)
                    | RefundLiabilities -- ^ Liability: Refund liabilities (返金負債)
                    | NonOperatingNotesPayable -- ^ Liability: Non-operating notes payable (営業外支払手形)
                    | NonOperatingElectronicallyRecordedObligations -- ^ Liability: Electronically recorded obligations - non-operating (営業外電子記録債務)
                    | BonusesPayable -- ^ Liability: Accrued bonuses (未払賞与)
                    | AllowanceForRepairs -- ^ Liability: Provision for repairs (修繕引当金)
                    | AllowanceForProductWarranties -- ^ Liability: Provision for warranties (商品保証引当金)
                    | AllowanceForBonuses -- ^ Liability: Provision for bonuses (賞与引当金)
                    | DeferredTaxLiabilities -- ^ Liability: Deferred tax liabilities (繰延税金負債)
                    | LeaseObligations -- ^ Liability: Lease liabilities (リース債務)
                    | GuaranteeDepositsReceived -- ^ Liability: Guarantee deposits received (預り保証金)
                    | AllowanceForRetirementBenefits -- ^ Liability: Provision for retirement benefits (退職給付引当金)
                    | LongTermOtherPayables -- ^ Liability: Long-term accounts payable - other (長期未払金)
                    | NetDefinedBenefitLiability -- ^ Liability: Retirement benefit liability (退職給付に係る負債)
                    | StockSubscriptionDeposits -- ^ Equity: Deposits for subscriptions of shares (株式申込証拠金)
                    | LegalCapitalSurplus -- ^ Equity: Legal capital surplus (資本準備金)
                    | OtherCapitalSurplus -- ^ Equity: Other capital surplus (その他資本剰余金)
                    | DividendEqualizationReserve -- ^ Equity: Reserve for dividend equalization (配当平均積立金)
                    | RepairFundReserve -- ^ Equity: Reserve for repairs (修繕積立金)
                    | ConstructionFundReserve -- ^ Equity: Reserve for new construction (新築積立金)
                    | GeneralReserve -- ^ Equity: General reserve (別途積立金)
                    | ValuationDifferenceOnOtherSecurities -- ^ Equity: Valuation difference on available-for-sale securities (その他有価証券評価差額金)
                    | NonControllingInterests -- ^ Equity: Non-controlling interests (非支配株主持分)
                    | CapitalSurplus -- ^ Equity: Capital surplus (資本剰余金)
                    | EarnedSurplus -- ^ Equity: Retained earnings (利益剰余金)
                    | ServiceRevenue -- ^ Revenue: Service revenue (役務収益)
                    | OperatingRevenue -- ^ Revenue: Operating revenue (営業収益)
                    | GainOnSalesOfSecurities -- ^ Revenue: Gain on sale of securities (有価証券売却益)
                    | GainOnValuationOfSecurities -- ^ Revenue: Gain on valuation of securities (有価証券評価益)
                    | DividendsReceived -- ^ Revenue: Dividend income (受取配当金)
                    | InterestOnSecurities -- ^ Revenue: Interest on securities (有価証券利息)
                    | GainOnSalesOfInvestmentSecurities -- ^ Revenue: Gain on sale of investment securities (投資有価証券売却益)
                    | InsuranceGain -- ^ Revenue: Gain on insurance claims (保険差益)
                    | GainOnBargainPurchase -- ^ Revenue: Gain on bargain purchase (負ののれん発生益)
                    | ReversalOfAllowanceForRepairs -- ^ Revenue: Reversal of provision for repairs (修繕引当金戻入)
                    | ReversalOfAllowanceForProductWarranties -- ^ Revenue: Reversal of provision for warranties (商品保証引当金戻入)
                    | GainOnDonationOfFixedAssets -- ^ Revenue: Gain on receipt of donated non-current assets (固定資産受贈益)
                    | GainOnNationalSubsidies -- ^ Revenue: Gain on receipt of national subsidies (国庫補助金受贈益)
                    | GainOnConstructionGrants -- ^ Revenue: Gain on contribution received for construction (工事負担金受贈益)
                    | LandRentReceived -- ^ Revenue: Rental income from land (受取地代)
                    | SalesRebates -- ^ Revenue: Sales rebates (売上割戻)
                    | CostOfServices -- ^ Cost: Cost of services (役務原価)
                    | OperatingExpenses -- ^ Cost: Operating expenses (営業費用)
                    | InventoryShrinkageLoss -- ^ Cost: Inventory shrinkage loss (棚卸減耗損)
                    | LossOnValuationOfMerchandise -- ^ Cost: Valuation loss on goods (商品評価損)
                    | Bonuses -- ^ Cost: Bonuses (賞与)
                    | RetirementBenefitExpenses -- ^ Cost: Retirement benefit expenses (退職給付費用)
                    | ProvisionForRepairs -- ^ Cost: Provision for repairs (修繕引当金繰入)
                    | ProvisionForBonuses -- ^ Cost: Provision for bonuses (賞与引当金繰入)
                    | ProvisionForProductWarranties -- ^ Cost: Provision for warranties (商品保証引当金繰入)
                    | ResearchAndDevelopmentExpenses -- ^ Cost: Research and development expenses (研究開発費)
                    | AmortizationOfGoodwill -- ^ Cost: Amortization of goodwill (のれん償却)
                    | AmortizationOfSoftware -- ^ Cost: Amortization of software (ソフトウェア償却)
                    | AmortizationOfPatents -- ^ Cost: Amortization of patent rights (特許権償却)
                    | LeaseExpenses -- ^ Cost: Lease expenses (支払リース料)
                    | IncorporationExpenses -- ^ Cost: Organization expenses (創立費)
                    | StockIssuanceCosts -- ^ Cost: Share issuance costs (株式交付費)
                    | BusinessCommencementExpenses -- ^ Cost: Business commencement expenses (開業費)
                    | DevelopmentExpenses -- ^ Cost: Development expenses (開発費)
                    | LossOnSalesOfElectronicallyRecordedReceivables -- ^ Cost: Loss on sale of electronically recorded monetary claims (電子記録債権売却損)
                    | LossOnSalesOfReceivables -- ^ Cost: Loss on sale of receivables (債権売却損)
                    | LossOnSalesOfSecurities -- ^ Cost: Loss on sale of securities (有価証券売却損)
                    | LossOnValuationOfSecurities -- ^ Cost: Loss on valuation of securities (有価証券評価損)
                    | LossOnSalesOfInvestmentSecurities -- ^ Cost: Loss on sale of investment securities (投資有価証券売却損)
                    | LossOnFire -- ^ Cost: Loss on fire (火災損失)
                    | LossOnRetirementOfFixedAssets -- ^ Cost: Loss on retirement of non-current assets (固定資産除却損)
                    | LossOnReductionOfFixedAssets -- ^ Cost: Loss on tax purpose reduction entry of non-current assets (固定資産圧縮損)
                    | AdditionalIncomeTaxesForPriorPeriods -- ^ Cost: Additional income taxes for prior periods (追徴法人税等)
                    | RefundOfIncomeTaxes -- ^ Cost: Refund of income taxes (還付法人税等)
                    | PurchaseRebates -- ^ Cost: Purchase rebates (仕入割戻)
                    | WelfareExpenses -- ^ Cost: Welfare expenses (福利厚生費)
                    | MaintenanceExpenses -- ^ Cost: Maintenance expenses (保守費)
                    | StatutoryWelfareExpenses -- ^ Cost: Legal welfare expenses (法定福利費)
                    | LandRentPaid -- ^ Cost: Rent expenses on land (支払地代)
                    | InsuranceExpense -- ^ Cost: Insurance expenses (保険料)
                    | RepairsExpense -- ^ Cost: Repair expenses (修繕費)
                    | StorageExpenses -- ^ Cost: Storage costs (保管費)
                    | MembershipFees -- ^ Cost: Membership fees (諸会費)
                    | IncomeSummary -- ^ Assets: Income summary (損益)
                    | SuspenseAccount -- ^ Assets: Suspense account (未決算)
                    | ForeignExchangeGains -- ^ Revenue: Foreign exchange gains (為替差益)
                    | ForeignExchangeLosses -- ^ Cost: Foreign exchange losses (為替差損)
                    | ContraAccountForGuaranteeObligations -- ^ Assets: Contra account for guarantee obligations (保証債務見返)
                    | GuaranteeObligations -- ^ Liability: Guarantee obligations (保証債務)
                    | IncomeTaxesAdjustment -- ^ Cost: Income taxes - deferred (法人税等調整額)
                    | BranchCurrentAccount -- ^ Assets: Branch current account (支店)
                    | HeadOfficeCurrentAccount -- ^ Assets: Head office current account (本店)
                    | NetIncomeAttributableToNCI -- ^ Cost: Profit attributable to non-controlling interests (非支配株主に帰属する当期純利益)
                    | NetLossAttributableToNCI -- ^ Revenue: Loss attributable to non-controlling interests (非支配株主に帰属する当期純損失)
                    | TradingSecurities -- ^ Assets: Trading securities (売買目的有価証券)
                    | HeldToMaturityBonds -- ^ Assets: Held-to-maturity bonds (満期保有目的債券)
                    | SubsidiaryStocks -- ^ Assets: Shares of subsidiaries (子会社株式)
                    | AffiliateStocks -- ^ Assets: Shares of associates (関連会社株式)
                    | AvailableForSaleSecurities -- ^ Assets: Available-for-sale securities (その他有価証券)
                    | AccountTitle                    -- ^ Wildcard (ワイルドカード — 任意の科目にマッチ。projWithBase 等の問い合わせ用で, 実 posting には使わない)
                    deriving (Show, Ord, Eq, Enum, Generic, Bounded)

-- Commutation の実用法は examples の文脈から「通信費」であることが確認済
-- (2026-06-11 調査)。Enum/Binary 序数の安定のため削除はせず, 新名称への移行を促す。
{-# DEPRECATED Commutation "通信費 (communication expenses) — use 'CommunicationExpenses' instead" #-}

instance Hashable AccountTitles where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt x = hashWithSalt salt (fromEnum x)

instance Binary.Binary AccountTitles where
    {-# INLINE put #-}
    put = BinaryPut.putWord16be . fromIntegral . fromEnum
    {-# INLINE get #-}
    get = do
        tag <- BinaryGet.getWord16be
        let maxTag = fromIntegral (fromEnum (maxBound :: AccountTitles))
        if tag <= maxTag
            then pure (toEnum (fromIntegral tag))
            else fail ("AccountTitles: invalid binary tag " ++ show tag)

instance Element AccountTitles where
    {-# INLINE wildcard #-}
    wildcard = AccountTitle



-- | Name :: Name of an item
type Name = Text

-- | Subject of an account title
type Subject = Text
instance Element Text where

    {-# INLINE wildcard #-}
    wildcard   = T.empty

-- | Currency unit or physical quantity
data CountUnit  = Yen
                | Dollar
                | Euro
                | CNY
                | Amount
                | CountUnit
                deriving (Show, Ord, Eq, Enum,Generic)

instance Hashable CountUnit where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt x = hashWithSalt salt (fromEnum x)

instance Binary.Binary CountUnit where
    {-# INLINE put #-}
    put = Binary.putWord8 . fromIntegral . fromEnum
    {-# INLINE get #-}
    get = toEnum . fromIntegral <$> Binary.getWord8

instance Element CountUnit where

    {-# INLINE wildcard #-}
    wildcard = CountUnit


-- TimeOfDay internally holds hour, minute, and second (Pico), so each is hashed individually
instance Hashable TimeOfDay where
  hashWithSalt salt (TimeOfDay hour minute sec) =
    salt `hashWithSalt` hour `hashWithSalt` minute `hashWithSalt` sec

-- Day internally holds an Integer in ModifiedJulianDay format, so that is used for hashing
instance Hashable Day where
  hashWithSalt salt day = hashWithSalt salt (toModifiedJulianDay day)

instance Element TimeOfDay where
    wildcard = Time.midnight

instance Element Day where
    wildcard =  ModifiedJulianDay 0

instance (Element a ,Element b)
    => Element (a, b) where

    {-# INLINE wildcard #-}
    wildcard = (wildcard, wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b)
        = isWildcard a
       || isWildcard b

    {-# INLINE equal #-}
    equal (a1, a2) (b1, b2)
        =  (a1 .== b1)
        && (a2 .== b2)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2) (b1, b2)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2)

    {-# INLINE compareElement #-}
    compareElement (a1, a2) (b1, b2)
        = case compareElement a1 b1 of
            EQ -> compareElement a2 b2
            x  -> x

instance {-# OVERLAPPING #-} (Element a, Element b)
    => AxisDecompose (a, b) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b) = [AxisKey a, AxisKey b]

instance (Element a, Element b, Element c)
    => Element (a, b, c) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c)
        = isWildcard a
       || isWildcard b
       || isWildcard c


    {-# INLINE equal #-}
    equal (a1, a2, a3) (b1, b2, b3)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3) (b1, b2, b3)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3) (b1, b2, b3)
        = compareElement ((a1, a2), a3)
                         ((b1, b2), b3)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c)
    => AxisDecompose (a, b, c) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c) = [AxisKey a, AxisKey b, AxisKey c]


instance (Element a, Element b, Element c, Element d)
    => Element (a, b, c, d) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard)


    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d)
        = isWildcard a
       || isWildcard b
       || isWildcard c
       || isWildcard d

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4) (b1, b2, b3, b4)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3, a4) (b1, b2, b3, b4)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3
          , ignoreWildcard a4 b4)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4) (b1, b2, b3, b4)
        = compareElement ((a1, a2, a3), a4)
                         ((b1, b2, b3), b4)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d)
    => AxisDecompose (a, b, c, d) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d]


instance (Element a, Element b, Element c, Element d, Element e)
    => Element (a, b, c, d, e) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard)


    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d,e)
        = isWildcard a
       || isWildcard b
       || isWildcard c
       || isWildcard d
       || isWildcard e

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3
          , ignoreWildcard a4 b4
          , ignoreWildcard a5 b5)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = compareElement ((a1, a2, a3, a4), a5)
                         ((b1, b2, b3, b4), b5)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e)
    => AxisDecompose (a, b, c, d, e) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e]


instance (Element a, Element b, Element c, Element d, Element e, Element f)
    => Element (a, b, c, d, e, f) where

    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d,e,f)
        = isWildcard a
       || isWildcard b
       || isWildcard c
       || isWildcard d
       || isWildcard e
       || isWildcard f

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3
          , ignoreWildcard a4 b4
          , ignoreWildcard a5 b5
          , ignoreWildcard a6 b6)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = compareElement ((a1, a2, a3, a4, a5), a6)
                         ((b1, b2, b3, b4, b5), b6)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e, Element f)
    => AxisDecompose (a, b, c, d, e, f) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e, f) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e, AxisKey f]


instance (Element a, Element b, Element c, Element d, Element e, Element f, Element g)
    => Element (a, b, c, d, e, f, g) where
    {-# INLINE wildcard #-}
    wildcard = ( wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard
                , wildcard)

    {-# INLINE haveWildcard #-}
    haveWildcard (a,b,c,d,e,f,g)
        = isWildcard a
       || isWildcard b
       || isWildcard c
       || isWildcard d
       || isWildcard e
       || isWildcard f
       || isWildcard g

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)
        && (a7 .== b7)

    {-# INLINE ignoreWildcard #-}
    ignoreWildcard (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = ( ignoreWildcard a1 b1
          , ignoreWildcard a2 b2
          , ignoreWildcard a3 b3
          , ignoreWildcard a4 b4
          , ignoreWildcard a5 b5
          , ignoreWildcard a6 b6
          , ignoreWildcard a7 b7)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = compareElement ((a1, a2, a3, a4, a5, a6), a7)
                         ((b1, b2, b3, b4, b5, b6), b7)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e, Element f, Element g)
    => AxisDecompose (a, b, c, d, e, f, g) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e, f, g) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e, AxisKey f, AxisKey g]
