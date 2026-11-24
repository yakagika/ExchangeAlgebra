{-# OPTIONS_GHC -Werror=incomplete-patterns -Werror=missing-fields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE DeriveGeneric              #-}

-- | This module is large because it keeps the complete account title table in one place.
--
-- To add an account title, add one constructor line and one equation for 'row'.
-- Assign an unused wire code and keep retired codes such as @0x8023@ reserved.
-- For a title listed in the JCCI 2022 fixture, enter its aliases in fixture order.
module ExchangeAlgebra.Accounting.Account.Title
    ( AccountTitles(..)
    , Row(..)
    , row
    ) where

import ExchangeAlgebra.Algebra.Element.Representation (Element(..))
import ExchangeAlgebra.Algebra.Base.Representation (BaseClass(..))
import GHC.Generics (Generic)
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData(..))
import Data.Text (Text)
import Data.Word (Word16)
import ExchangeAlgebra.Accounting.Account.Classification
import ExchangeAlgebra.Accounting.Account.Spec (AccountSpec(..), AccountSemantics(..))
import qualified Data.Map.Strict as Map
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as BinaryGet
import qualified Data.Binary.Put as BinaryPut

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
                    | EquityInEarningsOfInvestee      -- ^ Revenue: Equity in earnings of investee
                    -- (持分法による投資利益). Recognized under the equity method.
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
                    | MerchandiseInventory            -- ^ Asset: Merchandise inventory (繰越商品). Use under the periodic/3-account method (3 分法: Purchases\/Sales\/MerchandiseInventory). For 分記法, which records the cost of the goods and the gain on sales at each sale, use 'Products' instead.
                    | AdvancesPaid                    -- ^ Asset: Advances paid (前払金)
                    | PrepaidExpenses                 -- ^ Asset: Prepaid expenses (前払費用), deferral account (経過勘定)
                    | AccruedRevenue                  -- ^ Asset: Accrued revenue (未収収益), accrual account (経過勘定)
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
                    | UnearnedRevenue                 -- ^ Liability: Unearned revenue (前受収益), deferral account (経過勘定)
                    | AccruedExpenses                 -- ^ Liability: Accrued expenses (未払費用), accrual account (経過勘定)
                    | OtherPayables                   -- ^ Liability: Other payables (未払金)
                    | DepositsReceived                -- ^ Liability: Deposits received (預り金)
                    | SuspenseReceipts                -- ^ Liability: Suspense receipts (仮受金)
                    | ConsumptionTaxReceived          -- ^ Liability: Consumption tax received (仮受消費税)
                    | AccruedConsumptionTax           -- ^ Liability: Accrued (unpaid) consumption tax (未払消費税)
                    | AccruedCorporateIncomeTaxes     -- ^ Liability: Accrued (unpaid) corporate income taxes (未払法人税等)
                    | UnpaidDividends                 -- ^ Liability: Unpaid dividends (未払配当金)
                    | AllowanceForDoubtfulAccounts    -- ^ Asset (contra): Allowance for doubtful accounts (貸倒引当金), a credit-balance valuation account (評価勘定) deducted from receivables. Division is Assets with @isContra = True@ (Definition 7 amendment, 0.5.0.0); home side stays Credit, values stay non-negative, Hat\/Not structure intact. B\/S deduction (net) presentation is the ExchangeAlgebra.IO.Output module's job.
                    | AccumulatedDepreciation         -- ^ Asset (contra): Accumulated depreciation (減価償却累計額), valuation account (評価勘定) under the indirect method (間接法), deducted from the related depreciable assets. Division is Assets with @isContra = True@ for the same reason as 'AllowanceForDoubtfulAccounts'. This is the canonical bookkeeping account for accumulated depreciation; the existing 'ReserveForDepreciation' is retained as the legacy SNA\/macro-accounting name.
                    -- Equity (資本)
                    | LegalRetainedEarnings           -- ^ Equity: Legal (appropriated) retained earnings reserve (利益準備金)
                    | CumulativeTranslationAdjustment -- ^ Equity: Cumulative translation adjustment (為替換算調整勘定; 在外子会社等の外貨建財務諸表の換算差額を計上する OCI/資本の部の項目)
                    -- Cost (費用)
                    | ProvisionForDoubtfulAccounts    -- ^ Expense: Provision for doubtful accounts (貸倒引当金繰入)
                    | BadDebtLoss                     -- ^ Expense: Bad debt loss (貸倒損失)
                    | LossOnSalesOfFixedAssets        -- ^ Expense: Loss on sales of fixed assets (固定資産売却損)
                    | LossOnSalesOfNotesReceivable    -- ^ Expense: Loss on sales of notes receivable (手形売却損)
                    | PaymentFees                     -- ^ Expense: Payment fees / fees paid (支払手数料), the debit counterpart of 'ReceiptFee'
                    | MiscellaneousLoss               -- ^ Expense: Miscellaneous loss (雑損)
                    | CorporateIncomeTaxes            -- ^ Expense: Corporate income taxes (法人税等)
                    | CommunicationExpenses           -- ^ Expense: Communication expenses (通信費)
                    -- Revenue (収益)
                    | GainOnSalesOfFixedAssets        -- ^ Revenue: Gain on sales of fixed assets (固定資産売却益)
                    | RecoveryOfBadDebts              -- ^ Revenue: Recovery of bad debts written off (償却債権取立益)
                    | MiscellaneousIncome             -- ^ Revenue: Miscellaneous income (雑益)
                    | ReversalOfAllowanceForDoubtfulAccounts -- ^ Revenue: Reversal of allowance for doubtful accounts (貸倒引当金戻入). Credit counterpart used by the 差額補充法/洗替法 when the estimated allowance is smaller than the existing balance (the excess of 'AllowanceForDoubtfulAccounts' is released).
                    -- V-Land 2: JCCI grade-2 commercial-bookkeeping vocabulary.
                    | TimeDeposits -- ^ Asset: Time deposits (定期預金)
                    | LoansReceivable -- ^ Asset: Loans receivable (貸付金)
                    | GiftCertificatesReceived -- ^ Asset: Gift certificates received (受取商品券)
                    | SecurityDepositsPaid -- ^ Asset: Guarantee deposits (差入保証金)
                    | SuppliesOnHand -- ^ Asset: Supplies (貯蔵品)
                    | ContractAssets -- ^ Asset: Contract assets (契約資産)
                    | IncomeTaxesRefundReceivable -- ^ Asset: Income taxes refund receivable (未収還付法人税等)
                    | WorkInProcess -- ^ Asset: Work in process (仕掛品)
                    | DeferredTaxAssets -- ^ Asset: Deferred tax assets (繰延税金資産)
                    | LeasedAssets -- ^ Asset: Leased assets (リース資産)
                    | ToolsAndInstruments -- ^ Asset: Tools and instruments (工具器具)
                    | ConstructionInProgress -- ^ Asset: Construction in progress (建設仮勘定)
                    | Goodwill -- ^ Asset: Goodwill (のれん)
                    | SoftwareInProgress -- ^ Asset: Software in progress (ソフトウェア仮勘定)
                    | LongTermPrepaidExpenses -- ^ Asset: Long-term prepaid expenses (長期前払費用)
                    | DishonoredNotesReceivable -- ^ Asset: Dishonored notes receivable (不渡手形)
                    | PrepaidPensionCost -- ^ Asset: Prepaid pension costs (前払年金費用)
                    | NetDefinedBenefitAsset -- ^ Asset: Retirement benefit asset (退職給付に係る資産)
                    | DepositsInSpecialAccounts -- ^ Asset: Deposits in special accounts (別段預金)
                    | Structures -- ^ Asset: Structures (構築物)
                    | LeaseholdRights -- ^ Asset: Leasehold interests in land (借地権)
                    | NonOperatingNotesReceivable -- ^ Asset: Non-operating notes receivable (営業外受取手形)
                    | NonOperatingElectronicallyRecordedReceivable -- ^ Asset: Electronically recorded monetary claims - non-operating (営業外電子記録債権)
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
                    | CostOfServices -- ^ Expense: Cost of services (役務原価)
                    | OperatingExpenses -- ^ Expense: Operating expenses (営業費用)
                    | InventoryShrinkageLoss -- ^ Expense: Inventory shrinkage loss (棚卸減耗損)
                    | LossOnValuationOfMerchandise -- ^ Expense: Valuation loss on goods (商品評価損)
                    | Bonuses -- ^ Expense: Bonuses (賞与)
                    | RetirementBenefitExpenses -- ^ Expense: Retirement benefit expenses (退職給付費用)
                    | ProvisionForRepairs -- ^ Expense: Provision for repairs (修繕引当金繰入)
                    | ProvisionForBonuses -- ^ Expense: Provision for bonuses (賞与引当金繰入)
                    | ProvisionForProductWarranties -- ^ Expense: Provision for warranties (商品保証引当金繰入)
                    | ResearchAndDevelopmentExpenses -- ^ Expense: Research and development expenses (研究開発費)
                    | AmortizationOfGoodwill -- ^ Expense: Amortization of goodwill (のれん償却)
                    | AmortizationOfSoftware -- ^ Expense: Amortization of software (ソフトウェア償却)
                    | AmortizationOfPatents -- ^ Expense: Amortization of patent rights (特許権償却)
                    | LeaseExpenses -- ^ Expense: Lease expenses (支払リース料)
                    | IncorporationExpenses -- ^ Expense: Organization expenses (創立費)
                    | StockIssuanceCosts -- ^ Expense: Share issuance costs (株式交付費)
                    | BusinessCommencementExpenses -- ^ Expense: Business commencement expenses (開業費)
                    | DevelopmentExpenses -- ^ Expense: Development expenses (開発費)
                    | LossOnSalesOfElectronicallyRecordedReceivables -- ^ Expense: Loss on sale of electronically recorded monetary claims (電子記録債権売却損)
                    | LossOnSalesOfReceivables -- ^ Expense: Loss on sale of receivables (債権売却損)
                    | LossOnSalesOfSecurities -- ^ Expense: Loss on sale of securities (有価証券売却損)
                    | LossOnValuationOfSecurities -- ^ Expense: Loss on valuation of securities (有価証券評価損)
                    | LossOnSalesOfInvestmentSecurities -- ^ Expense: Loss on sale of investment securities (投資有価証券売却損)
                    | LossOnFire -- ^ Expense: Loss on fire (火災損失)
                    | LossOnRetirementOfFixedAssets -- ^ Expense: Loss on retirement of non-current assets (固定資産除却損)
                    | LossOnReductionOfFixedAssets -- ^ Expense: Loss on tax purpose reduction entry of non-current assets (固定資産圧縮損)
                    | AdditionalIncomeTaxesForPriorPeriods -- ^ Expense: Additional income taxes for prior periods (追徴法人税等)
                    | RefundOfIncomeTaxes -- ^ Expense: Refund of income taxes (還付法人税等)
                    | PurchaseRebates -- ^ Expense: Purchase rebates (仕入割戻)
                    | WelfareExpenses -- ^ Expense: Welfare expenses (福利厚生費)
                    | MaintenanceExpenses -- ^ Expense: Maintenance expenses (保守費)
                    | StatutoryWelfareExpenses -- ^ Expense: Legal welfare expenses (法定福利費)
                    | LandRentPaid -- ^ Expense: Rent expenses on land (支払地代)
                    | InsuranceExpense -- ^ Expense: Insurance expenses (保険料)
                    | RepairsExpense -- ^ Expense: Repair expenses (修繕費)
                    | StorageExpenses -- ^ Expense: Storage costs (保管費)
                    | MembershipFees -- ^ Expense: Membership fees (諸会費)
                    | IncomeSummary -- ^ Asset: Income summary (損益)
                    | SuspenseAccount -- ^ Asset: Suspense account (未決算)
                    | ForeignExchangeGains -- ^ Revenue: Foreign exchange gains (為替差益)
                    | ForeignExchangeLosses -- ^ Expense: Foreign exchange losses (為替差損)
                    | ContraAccountForGuaranteeObligations -- ^ Asset: Contra account for guarantee obligations (保証債務見返)
                    | GuaranteeObligations -- ^ Liability: Guarantee obligations (保証債務)
                    | IncomeTaxesAdjustment -- ^ Expense: Income taxes - deferred (法人税等調整額)
                    | BranchCurrentAccount -- ^ Asset: Branch current account (支店)
                    | HeadOfficeCurrentAccount -- ^ Asset: Head office current account (本店)
                    | NetIncomeAttributableToNCI -- ^ Expense: Profit attributable to non-controlling interests (非支配株主に帰属する当期純利益)
                    | NetLossAttributableToNCI -- ^ Revenue: Loss attributable to non-controlling interests (非支配株主に帰属する当期純損失)
                    | TradingSecurities -- ^ Asset: Trading securities (売買目的有価証券)
                    | HeldToMaturityBonds -- ^ Asset: Held-to-maturity bonds (満期保有目的債券)
                    | SubsidiaryStocks -- ^ Asset: Shares of subsidiaries (子会社株式)
                    | AffiliateStocks -- ^ Asset: Shares of associates (関連会社株式)
                    | AvailableForSaleSecurities -- ^ Asset: Available-for-sale securities (その他有価証券)
                    | ConsumptionTaxRefundReceivable -- ^ Asset: Consumption tax refund receivable (未収還付消費税)
                    | PropertyTaxPayable -- ^ Liability: Property tax payable (未払固定資産税)
                    | DepositsReceivedFromOfficers -- ^ Liability: Deposits received from officers (役員預り金)
                    | EntertainmentExpenses -- ^ Expense: Entertainment expenses (交際費)
                    | MeetingExpenses -- ^ Expense: Meeting expenses (会議費)
                    | NewspaperBooksExpenses -- ^ Expense: Newspapers and books (新聞図書費)
                    | RawMaterials -- ^ Asset: Raw materials (原材料)
                    | GoodsInTransit -- ^ Asset: Goods in transit (未着品)
                    | SalesAllowances -- ^ Revenue: Sales allowances (売上値引)
                    | PurchaseAllowances -- ^ Expense: Purchase allowances (仕入値引)
                    | ReversalOfAllowanceForBonuses
                      -- ^ Revenue: Reversal of provision for bonuses (賞与引当金戻入)
                    | DirectorsCompensations
                      -- ^ Expense: Remuneration for directors (役員報酬)
                    | AccountTitle                    -- ^ Wildcard (ワイルドカード — 任意の科目にマッチ。projWithBase 等の問い合わせ用で, 実 posting には使わない)
                    deriving (Show, Ord, Eq, Enum, Generic, Bounded)

-- | Account title schemes covered by the table.
data Scheme = Jcci | Macro

-- | Wire code and, for a concrete account title, its metadata and aliases.
data Row
    = WildcardRow
    | ConcreteRow
        { rowCode        :: Word16
        , rowScheme      :: Scheme
        , rowSpec        :: ~AccountSpec
        , rowSemantics   :: ~AccountSemantics
        , rowJcciAliases :: ~[Text]
        }

-- | Look up the row for any account title.
row :: AccountTitles -> Row
row Cash = ConcreteRow
    { rowCode = 0x8000
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Cash"
        , asNameJa = "現金"
        , asLabelJa = "現金"
        , asDescription = "Asset: Cash (現金)"
        , asAliases = ["現金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["現金"]
    }
row Deposits = ConcreteRow
    { rowCode = 0x8001
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Savings deposits"
        , asNameJa = "普通預金"
        , asLabelJa = "普通預金"
        , asDescription = "Asset: Savings deposits (普通預金)"
        , asAliases = ["普通預金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["普通預金", "銀行預金", "普通預金○○銀行"]
    }
row CurrentDeposits = ConcreteRow
    { rowCode = 0x8002
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Current deposits"
        , asNameJa = "当座預金"
        , asLabelJa = "当座預金"
        , asDescription = "Asset: Current deposits (当座預金)"
        , asAliases = ["当座預金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["当座預金", "当座預金○○銀行"]
    }
row Securities = ConcreteRow
    { rowCode = 0x8003
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Securities"
        , asNameJa = "有価証券"
        , asLabelJa = "有価証券"
        , asDescription = "Asset: Securities (有価証券)"
        , asAliases = ["有価証券"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["有価証券"]
    }
row InvestmentSecurities = ConcreteRow
    { rowCode = 0x8004
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Investment securities"
        , asNameJa = "投資有価証券"
        , asLabelJa = "投資有価証券"
        , asDescription = "Asset: Investment securities (投資有価証券)"
        , asAliases = ["投資有価証券"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["投資有価証券"]
    }
row InvestmentInAssociate = ConcreteRow
    { rowCode = 0x8005
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Investment in associate"
        , asNameJa = "関係会社株式"
        , asLabelJa = "関係会社株式"
        , asDescription = "Asset: Investment in associate (関係会社株式). Carrying amount under the equity method (持分法適用投資勘定)."
        , asAliases = []
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row LongTermNationalBonds = ConcreteRow
    { rowCode = 0x8006
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Long-term national bonds"
        , asNameJa = "長期国債"
        , asLabelJa = "長期国債"
        , asDescription = "Asset: Long-term national bonds (長期国債)"
        , asAliases = ["長期国債"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row ShortTermNationalBonds = ConcreteRow
    { rowCode = 0x8007
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Short-term national bonds"
        , asNameJa = "短期国債"
        , asLabelJa = "短期国債"
        , asDescription = "Asset: Short-term national bonds (短期国債)"
        , asAliases = ["短期国債"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row Products = ConcreteRow
    { rowCode = 0x8008
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Products"
        , asNameJa = "商品"
        , asLabelJa = "商品"
        , asDescription = "Asset: Products (商品)。分記法用。3 分法 (仕入/売上/繰越商品) では 'MerchandiseInventory' を使う"
        , asAliases = ["商品"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["商品"]
    }
row Machinery = ConcreteRow
    { rowCode = 0x8009
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Machinery and equipment"
        , asNameJa = "機械装置"
        , asLabelJa = "機械装置"
        , asDescription = "Asset: Machinery and equipment (機械装置)"
        , asAliases = ["機械装置"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["機械装置", "機械"]
    }
row Building = ConcreteRow
    { rowCode = 0x800A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Real estate"
        , asNameJa = "建物"
        , asLabelJa = "建物"
        , asDescription = "Asset: Real estate (建物)"
        , asAliases = ["建物"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["建物"]
    }
row Vehicle = ConcreteRow
    { rowCode = 0x800B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Vehicles"
        , asNameJa = "車両運搬具"
        , asLabelJa = "車両運搬具"
        , asDescription = "Asset: Vehicles (車両運搬具)"
        , asAliases = ["車両運搬具"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["車両運搬具", "車両", "運搬具"]
    }
row StockInvestment = ConcreteRow
    { rowCode = 0x800C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Stock investment"
        , asNameJa = "株式投資"
        , asLabelJa = "株式投資"
        , asDescription = "Asset: Stock investment (株式投資)"
        , asAliases = ["株式投資"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row EquipmentInvestment = ConcreteRow
    { rowCode = 0x800D
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Equipment investment"
        , asNameJa = "設備投資"
        , asLabelJa = "設備投資"
        , asDescription = "Asset: Equipment investment (設備投資)"
        , asAliases = ["設備投資"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row LongTermLoansReceivable = ConcreteRow
    { rowCode = 0x800E
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Long-term loans receivable"
        , asNameJa = "長期貸付金"
        , asLabelJa = "長期貸付金"
        , asDescription = "Asset: Long-term loans receivable (長期貸付金)"
        , asAliases = ["長期貸付金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["長期貸付金"]
    }
row AccountsReceivable = ConcreteRow
    { rowCode = 0x800F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Accounts receivable"
        , asNameJa = "売掛金"
        , asLabelJa = "売掛金"
        , asDescription = "Asset: Accounts receivable (売掛金)"
        , asAliases = ["売掛金", "a/r", "accounts receivable"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["売掛金", "〇〇商店"]
    }
row ShortTermLoansReceivable = ConcreteRow
    { rowCode = 0x8010
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Short-term loans receivable"
        , asNameJa = "短期貸付金"
        , asLabelJa = "短期貸付金"
        , asDescription = "Asset: Short-term loans receivable (短期貸付金)"
        , asAliases = ["短期貸付金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["短期貸付金"]
    }
row ReserveDepositReceivable = ConcreteRow
    { rowCode = 0x8011
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Reserve deposits, asset side"
        , asNameJa = "準備預金, 資産側 — 市中銀行が中央銀行に置く準備預金。SNA/マクロ系"
        , asLabelJa = "準備預金"
        , asDescription = "Asset: Reserve deposits, asset side (準備預金, 資産側 — 市中銀行が中央銀行に置く準備預金。SNA/マクロ系)"
        , asAliases = ["準備預金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row Gold = ConcreteRow
    { rowCode = 0x8012
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Gold"
        , asNameJa = "金"
        , asLabelJa = "金"
        , asDescription = "Asset: Gold (金)"
        , asAliases = ["金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row GovernmentService = ConcreteRow
    { rowCode = 0x8013
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Government service"
        , asNameJa = "政府サービス。SNA/マクロ系"
        , asLabelJa = "政府サービス"
        , asDescription = "Asset: Government service (政府サービス。SNA/マクロ系)"
        , asAliases = ["政府サービス"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row CapitalStock = ConcreteRow
    { rowCode = 0x8014
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Capital stock"
        , asNameJa = "資本金"
        , asLabelJa = "資本金"
        , asDescription = "Equity: Capital stock (資本金)"
        , asAliases = ["資本金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["資本金"]
    }
row RetainedEarnings = ConcreteRow
    { rowCode = 0x8015
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Retained earnings"
        , asNameJa = "繰越利益剰余金"
        , asLabelJa = "繰越利益剰余金"
        , asDescription = "Equity: Retained earnings (繰越利益剰余金)"
        , asAliases = ["繰越利益剰余金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["繰越利益剰余金"]
    }
row LongTermLoansPayable = ConcreteRow
    { rowCode = 0x8016
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Long-term loans payable"
        , asNameJa = "長期借入金"
        , asLabelJa = "長期借入金"
        , asDescription = "Liability: Long-term loans payable (長期借入金)"
        , asAliases = ["長期借入金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["長期借入金"]
    }
row ShortTermLoansPayable = ConcreteRow
    { rowCode = 0x8017
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Short-term loans payable"
        , asNameJa = "短期借入金"
        , asLabelJa = "短期借入金"
        , asDescription = "Liability: Short-term loans payable (短期借入金)"
        , asAliases = ["短期借入金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["短期借入金"]
    }
row LoansPayable = ConcreteRow
    { rowCode = 0x8018
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Loans payable"
        , asNameJa = "借入金"
        , asLabelJa = "借入金"
        , asDescription = "Liability: Loans payable (借入金)"
        , asAliases = ["借入金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["借入金", "銀行借入金", "役員借入金"]
    }
row ReserveForDepreciation = ConcreteRow
    { rowCode = 0x8019
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Reserve for depreciation"
        , asNameJa = "減価償却引当金 — SNA/マクロ系の旧称。簿記の間接法には 'AccumulatedDepreciation' (減価償却累計額) を使う"
        , asLabelJa = "減価償却引当金"
        , asDescription = "Liability: Reserve for depreciation (減価償却引当金 — SNA/マクロ系の旧称。簿記の間接法には 'AccumulatedDepreciation' (減価償却累計額) を使う)"
        , asAliases = ["減価償却引当金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row DepositPayable = ConcreteRow
    { rowCode = 0x801A
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Deposits accepted"
        , asNameJa = "受入預金 — 銀行側の負債としての預金。SNA/マクロ系。従業員等からの預り金は 'DepositsReceived'"
        , asLabelJa = "受入預金"
        , asDescription = "Liability: Deposits accepted (受入預金 — 銀行側の負債としての預金。SNA/マクロ系。従業員等からの預り金は 'DepositsReceived')"
        , asAliases = ["受入預金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row LongTermNationalBondsPayable = ConcreteRow
    { rowCode = 0x801B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Long-term national bonds payable"
        , asNameJa = "長期国債, 発行側"
        , asLabelJa = "長期国債"
        , asDescription = "Liability: Long-term national bonds payable (長期国債, 発行側)"
        , asAliases = ["長期国債"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row ShortTermNationalBondsPayable = ConcreteRow
    { rowCode = 0x801C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Short-term national bonds payable"
        , asNameJa = "短期国債, 発行側"
        , asLabelJa = "短期国債"
        , asDescription = "Liability: Short-term national bonds payable (短期国債, 発行側)"
        , asAliases = ["短期国債"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row ReserveDepositPayable = ConcreteRow
    { rowCode = 0x801D
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Reserve deposits, liability side"
        , asNameJa = "準備預金, 負債側 — 中央銀行が受け入れる準備預金。SNA/マクロ系"
        , asLabelJa = "準備預金"
        , asDescription = "Liability: Reserve deposits, liability side (準備預金, 負債側 — 中央銀行が受け入れる準備預金。SNA/マクロ系)。※簿記の買掛金は 'AccountsPayable' を使うこと (旧 examples が本科目を買掛金の代用にしていた経緯あり)"
        , asAliases = ["準備預金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row CentralBankNotePayable = ConcreteRow
    { rowCode = 0x801E
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Central bank notes"
        , asNameJa = "発行銀行券。SNA/マクロ系"
        , asLabelJa = "発行銀行券"
        , asDescription = "Liability: Central bank notes (発行銀行券。SNA/マクロ系)"
        , asAliases = ["発行銀行券"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row Depreciation = ConcreteRow
    { rowCode = 0x801F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Depreciation"
        , asNameJa = "減価償却費"
        , asLabelJa = "減価償却費"
        , asDescription = "Expense: Depreciation (減価償却費)"
        , asAliases = ["減価償却費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["減価償却費", "建物減価償却費", "備品減価償却費"]
    }
row AmortizationExpense = ConcreteRow
    { rowCode = 0x8020
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Amortization expense for intangibles"
        , asNameJa = "無形固定資産償却費 — 特許権・商標権・ソフトウェア等の無形資産の償却費。有形の 'Depreciation' (減価償却費) と区別される"
        , asLabelJa = "無形固定資産償却費"
        , asDescription = "Expense: Amortization expense for intangibles (無形固定資産償却費 — 特許権・商標権・ソフトウェア等の無形資産の償却費。有形の 'Depreciation' (減価償却費) と区別される)"
        , asAliases = []
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row SalesCost = ConcreteRow
    { rowCode = 0x8021
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Cost of sales"
        , asNameJa = "売上原価"
        , asLabelJa = "売上原価"
        , asDescription = "Expense: Cost of sales (売上原価)"
        , asAliases = ["売上原価", "cogs", "cost of sales"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["売上原価"]
    }
row BusinessTrip = ConcreteRow
    { rowCode = 0x8022
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Travel and transportation"
        , asNameJa = "旅費交通費"
        , asLabelJa = "旅費交通費"
        , asDescription = "Expense: Travel and transportation (旅費交通費)"
        , asAliases = ["旅費交通費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["旅費交通費", "旅費", "交通費"]
    }
row UtilitiesExpense = ConcreteRow
    { rowCode = 0x8024
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Utilities"
        , asNameJa = "水道光熱費"
        , asLabelJa = "水道光熱費"
        , asDescription = "Expense: Utilities (水道光熱費)"
        , asAliases = ["水道光熱費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["水道光熱費", "光熱水費"]
    }
row RentExpense = ConcreteRow
    { rowCode = 0x8025
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Rent"
        , asNameJa = "支払家賃"
        , asLabelJa = "支払家賃"
        , asDescription = "Expense: Rent (支払家賃)"
        , asAliases = ["支払家賃"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["支払家賃", "地代家賃", "支払賃借料", "支払不動産賃借料"]
    }
row AdvertisingExpense = ConcreteRow
    { rowCode = 0x8026
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Advertising"
        , asNameJa = "広告宣伝費"
        , asLabelJa = "広告宣伝費"
        , asDescription = "Expense: Advertising (広告宣伝費)"
        , asAliases = ["広告宣伝費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["広告宣伝費", "広告費", "広告料", "宣伝費"]
    }
row DeliveryExpenses = ConcreteRow
    { rowCode = 0x8027
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Delivery"
        , asNameJa = "発送費"
        , asLabelJa = "発送費"
        , asDescription = "Expense: Delivery (発送費)"
        , asAliases = ["発送費", "荷造運賃"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["発送費", "支払運賃", "発送運賃"]
    }
row SuppliesExpenses = ConcreteRow
    { rowCode = 0x8028
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Supplies"
        , asNameJa = "消耗品費"
        , asLabelJa = "消耗品費"
        , asDescription = "Expense: Supplies (消耗品費)"
        , asAliases = ["消耗品費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["消耗品費", "事務用消耗品費"]
    }
row MiscellaneousExpenses = ConcreteRow
    { rowCode = 0x8029
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Miscellaneous"
        , asNameJa = "雑費"
        , asLabelJa = "雑費"
        , asDescription = "Expense: Miscellaneous (雑費)"
        , asAliases = ["雑費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["雑費"]
    }
row WageExpenditure = ConcreteRow
    { rowCode = 0x802A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Wages"
        , asNameJa = "給料"
        , asLabelJa = "給料"
        , asDescription = "Expense: Wages (給料)"
        , asAliases = ["給料"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["給料", "給料手当", "賃金給料"]
    }
row InterestExpense = ConcreteRow
    { rowCode = 0x802B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Interest expense"
        , asNameJa = "支払利息"
        , asLabelJa = "支払利息"
        , asDescription = "Expense: Interest expense (支払利息)"
        , asAliases = ["支払利息"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["支払利息"]
    }
row TaxesExpense = ConcreteRow
    { rowCode = 0x802C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Taxes"
        , asNameJa = "租税公課"
        , asLabelJa = "租税公課"
        , asDescription = "Expense: Taxes (租税公課)"
        , asAliases = ["租税公課"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["租税公課", "公租公課", "固定資産税", "印紙税"]
    }
row ConsumptionExpenditure = ConcreteRow
    { rowCode = 0x802D
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Consumption expenditure"
        , asNameJa = "消費支出。SNA/マクロ系"
        , asLabelJa = "消費支出"
        , asDescription = "Expense: Consumption expenditure (消費支出。SNA/マクロ系)"
        , asAliases = ["消費支出"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row SubsidyExpense = ConcreteRow
    { rowCode = 0x802E
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Subsidy expenditure"
        , asNameJa = "補助金支出。SNA/マクロ系"
        , asLabelJa = "補助金支出"
        , asDescription = "Expense: Subsidy expenditure (補助金支出。SNA/マクロ系)"
        , asAliases = ["補助金支出"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row CentralBankPaymentExpense = ConcreteRow
    { rowCode = 0x802F
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Central bank payment to treasury"
        , asNameJa = "国庫納付金支出。SNA/マクロ系"
        , asLabelJa = "国庫納付金支出"
        , asDescription = "Expense: Central bank payment to treasury (国庫納付金支出。SNA/マクロ系)"
        , asAliases = ["国庫納付金支出"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row Purchases = ConcreteRow
    { rowCode = 0x8030
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Purchases"
        , asNameJa = "仕入"
        , asLabelJa = "仕入"
        , asDescription = "Expense: Purchases (仕入)"
        , asAliases = ["仕入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["仕入"]
    }
row NetIncome = ConcreteRow
    { rowCode = 0x8031
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = NoClose
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Net income"
        , asNameJa = "当期純利益 — 決算振替用。借方側に立つため Expense 区分"
        , asLabelJa = "当期純利益"
        , asDescription = "Expense: Net income (当期純利益 — 決算振替用。借方側に立つため Expense 区分)"
        , asAliases = ["当期純利益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [PeriodResult]
        , asemPostingCapability = EngineGeneratedOnly
        , asemDivisionSemantics = DirectionEncoding Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = DerivedPresentation
        }
    , rowJcciAliases = []
    }
row ValueAdded = ConcreteRow
    { rowCode = 0x8032
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Value added"
        , asNameJa = "付加価値。SNA/マクロ系"
        , asLabelJa = "付加価値"
        , asDescription = "Revenue: Value added (付加価値。SNA/マクロ系)"
        , asAliases = ["付加価値"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row SubsidyIncome = ConcreteRow
    { rowCode = 0x8033
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Subsidy income"
        , asNameJa = "補助金収入。SNA/マクロ系"
        , asLabelJa = "補助金収入"
        , asDescription = "Revenue: Subsidy income (補助金収入。SNA/マクロ系)"
        , asAliases = ["補助金収入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row NationalBondInterestEarned = ConcreteRow
    { rowCode = 0x8034
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "National bond interest earned"
        , asNameJa = "国債利息収入"
        , asLabelJa = "国債利息収入"
        , asDescription = "Revenue: National bond interest earned (国債利息収入)"
        , asAliases = ["国債利息収入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row DepositInterestEarned = ConcreteRow
    { rowCode = 0x8035
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Deposit interest earned"
        , asNameJa = "預金利息収入"
        , asLabelJa = "預金利息収入"
        , asDescription = "Revenue: Deposit interest earned (預金利息収入)"
        , asAliases = ["預金利息収入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row GrossProfit = ConcreteRow
    { rowCode = 0x8036
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gross profit"
        , asNameJa = "売上総利益 — 決算振替用"
        , asLabelJa = "売上総利益"
        , asDescription = "Revenue: Gross profit (売上総利益 — 決算振替用)"
        , asAliases = ["売上総利益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [ReportingSubtotal]
        , asemPostingCapability = EngineGeneratedOnly
        , asemDivisionSemantics = DirectionEncoding Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = DerivedPresentation
        }
    , rowJcciAliases = []
    }
row OrdinaryProfit = ConcreteRow
    { rowCode = 0x8037
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Ordinary profit"
        , asNameJa = "経常利益 — 決算振替用"
        , asLabelJa = "経常利益"
        , asDescription = "Revenue: Ordinary profit (経常利益 — 決算振替用)"
        , asAliases = ["経常利益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [ReportingSubtotal]
        , asemPostingCapability = EngineGeneratedOnly
        , asemDivisionSemantics = DirectionEncoding Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = DerivedPresentation
        }
    , rowJcciAliases = []
    }
row InterestEarned = ConcreteRow
    { rowCode = 0x8038
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Interest earned"
        , asNameJa = "受取利息"
        , asLabelJa = "受取利息"
        , asDescription = "Revenue: Interest earned (受取利息)"
        , asAliases = ["受取利息"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["受取利息"]
    }
row ReceiptFee = ConcreteRow
    { rowCode = 0x8039
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Receipt fee"
        , asNameJa = "受取手数料。支払側は 'PaymentFees'"
        , asLabelJa = "受取手数料"
        , asDescription = "Revenue: Receipt fee (受取手数料。支払側は 'PaymentFees')"
        , asAliases = ["受取手数料"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["受取手数料"]
    }
row RentalIncome = ConcreteRow
    { rowCode = 0x803A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Rental income"
        , asNameJa = "受取家賃"
        , asLabelJa = "受取家賃"
        , asDescription = "Revenue: Rental income (受取家賃)"
        , asAliases = ["受取家賃"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["受取家賃"]
    }
row WageEarned = ConcreteRow
    { rowCode = 0x803B
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Wage income"
        , asNameJa = "賃金収入。SNA/マクロ系"
        , asLabelJa = "賃金収入"
        , asDescription = "Revenue: Wage income (賃金収入。SNA/マクロ系)"
        , asAliases = ["賃金収入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row TaxesRevenue = ConcreteRow
    { rowCode = 0x803C
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Tax revenue"
        , asNameJa = "租税収入。SNA/マクロ系"
        , asLabelJa = "租税収入"
        , asDescription = "Revenue: Tax revenue (租税収入。SNA/マクロ系)"
        , asAliases = ["租税収入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row CentralBankPaymentIncome = ConcreteRow
    { rowCode = 0x803D
    , rowScheme = Macro
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Central bank payment to treasury"
        , asNameJa = "国庫納付金収入。SNA/マクロ系"
        , asLabelJa = "国庫納付金収入"
        , asDescription = "Revenue: Central bank payment to treasury (国庫納付金収入。SNA/マクロ系)"
        , asAliases = ["国庫納付金収入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row Sales = ConcreteRow
    { rowCode = 0x803E
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Sales"
        , asNameJa = "売上"
        , asLabelJa = "売上"
        , asDescription = "Revenue: Sales (売上)"
        , asAliases = ["売上"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["売上"]
    }
row EquityInEarningsOfInvestee = ConcreteRow
    { rowCode = 0x803F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Equity in earnings of investee"
        , asNameJa = "持分法による投資利益"
        , asLabelJa = "持分法による投資利益"
        , asDescription = "Revenue: Equity in earnings of investee (持分法による投資利益). Recognized under the equity method."
        , asAliases = []
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = ConsolidationOnly
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = []
    }
row NetLoss = ConcreteRow
    { rowCode = 0x8040
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = NoClose
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Net loss"
        , asNameJa = "当期純損失 — 決算振替用。貸方側に立つため Revenue 区分"
        , asLabelJa = "当期純損失"
        , asDescription = "Revenue: Net loss (当期純損失 — 決算振替用。貸方側に立つため Revenue 区分)"
        , asAliases = ["当期純損失"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [PeriodResult]
        , asemPostingCapability = EngineGeneratedOnly
        , asemDivisionSemantics = DirectionEncoding Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = DerivedPresentation
        }
    , rowJcciAliases = []
    }
row PettyCash = ConcreteRow
    { rowCode = 0x8041
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Petty cash"
        , asNameJa = "小口現金"
        , asLabelJa = "小口現金"
        , asDescription = "Asset: Petty cash (小口現金)"
        , asAliases = ["小口現金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["小口現金"]
    }
row NotesReceivable = ConcreteRow
    { rowCode = 0x8042
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Notes receivable"
        , asNameJa = "受取手形"
        , asLabelJa = "受取手形"
        , asDescription = "Asset: Notes receivable (受取手形)"
        , asAliases = ["受取手形"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["受取手形"]
    }
row ElectronicallyRecordedReceivable = ConcreteRow
    { rowCode = 0x8043
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Electronically recorded monetary claims"
        , asNameJa = "電子記録債権"
        , asLabelJa = "電子記録債権"
        , asDescription = "Asset: Electronically recorded monetary claims (電子記録債権)"
        , asAliases = ["電子記録債権"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["電子記録債権"]
    }
row CreditCardReceivable = ConcreteRow
    { rowCode = 0x8044
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Credit card receivable"
        , asNameJa = "クレジット売掛金"
        , asLabelJa = "クレジット売掛金"
        , asDescription = "Asset: Credit card receivable (クレジット売掛金)"
        , asAliases = ["クレジット売掛金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["クレジット売掛金"]
    }
row NotesLoansReceivable = ConcreteRow
    { rowCode = 0x8045
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Loans receivable on notes"
        , asNameJa = "手形貸付金"
        , asLabelJa = "手形貸付金"
        , asDescription = "Asset: Loans receivable on notes (手形貸付金)"
        , asAliases = ["手形貸付金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["貸付金", "手形貸付金"]
    }
row MerchandiseInventory = ConcreteRow
    { rowCode = 0x8046
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Merchandise inventory"
        , asNameJa = "繰越商品"
        , asLabelJa = "繰越商品"
        , asDescription = "Asset: Merchandise inventory (繰越商品). Use under the periodic/3-account method (3 分法: Purchases/Sales/MerchandiseInventory). For 分記法, which records the cost of the goods and the gain on sales at each sale, use 'Products' instead."
        , asAliases = ["繰越商品"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["繰越商品"]
    }
row AdvancesPaid = ConcreteRow
    { rowCode = 0x8047
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Advances paid"
        , asNameJa = "前払金"
        , asLabelJa = "前払金"
        , asDescription = "Asset: Advances paid (前払金)"
        , asAliases = ["前払金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["前払金", "前渡金"]
    }
row PrepaidExpenses = ConcreteRow
    { rowCode = 0x8048
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Prepaid expenses"
        , asNameJa = "前払費用"
        , asLabelJa = "前払費用"
        , asDescription = "Asset: Prepaid expenses (前払費用), deferral account (経過勘定)"
        , asAliases = ["前払費用"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["前払費用", "前払保険料"]
    }
row AccruedRevenue = ConcreteRow
    { rowCode = 0x8049
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Accrued revenue"
        , asNameJa = "未収収益"
        , asLabelJa = "未収収益"
        , asDescription = "Asset: Accrued revenue (未収収益), accrual account (経過勘定)"
        , asAliases = ["未収収益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未収収益", "未収家賃"]
    }
row OtherReceivables = ConcreteRow
    { rowCode = 0x804A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Other receivables"
        , asNameJa = "未収入金"
        , asLabelJa = "未収入金"
        , asDescription = "Asset: Other receivables (未収入金)"
        , asAliases = ["未収入金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未収入金", "未収金"]
    }
row PaymentsOnBehalf = ConcreteRow
    { rowCode = 0x804B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Payments made on behalf"
        , asNameJa = "立替金"
        , asLabelJa = "立替金"
        , asDescription = "Asset: Payments made on behalf (立替金)"
        , asAliases = ["立替金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["立替金", "従業員立替金"]
    }
row SuspensePayments = ConcreteRow
    { rowCode = 0x804C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Suspense payments"
        , asNameJa = "仮払金"
        , asLabelJa = "仮払金"
        , asDescription = "Asset: Suspense payments (仮払金)"
        , asAliases = ["仮払金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [SuspenseOrClearingAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = BookkeepingControlClass Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = ["仮払金"]
    }
row ConsumptionTaxPaid = ConcreteRow
    { rowCode = 0x804D
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Consumption tax paid"
        , asNameJa = "仮払消費税"
        , asLabelJa = "仮払消費税"
        , asDescription = "Asset: Consumption tax paid (仮払消費税)"
        , asAliases = ["仮払消費税"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["仮払金", "仮払消費税"]
    }
row PrepaidCorporateIncomeTaxes = ConcreteRow
    { rowCode = 0x804E
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Prepaid corporate income taxes"
        , asNameJa = "仮払法人税等"
        , asLabelJa = "仮払法人税等"
        , asDescription = "Asset: Prepaid corporate income taxes (仮払法人税等)"
        , asAliases = ["仮払法人税等"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["仮払金", "仮払法人税等"]
    }
row Land = ConcreteRow
    { rowCode = 0x804F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Land"
        , asNameJa = "土地"
        , asLabelJa = "土地"
        , asDescription = "Asset: Land (土地)"
        , asAliases = ["土地"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["土地"]
    }
row Fixtures = ConcreteRow
    { rowCode = 0x8050
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Fixtures and equipment"
        , asNameJa = "備品"
        , asLabelJa = "備品"
        , asDescription = "Asset: Fixtures and equipment (備品)"
        , asAliases = ["備品", "器具備品"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["備品"]
    }
row Patent = ConcreteRow
    { rowCode = 0x8051
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Patent"
        , asNameJa = "特許権"
        , asLabelJa = "特許権"
        , asDescription = "Asset: Patent (特許権)"
        , asAliases = ["特許権"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["特許権"]
    }
row Trademark = ConcreteRow
    { rowCode = 0x8052
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Trademark"
        , asNameJa = "商標権"
        , asLabelJa = "商標権"
        , asDescription = "Asset: Trademark (商標権)"
        , asAliases = ["商標権"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["商標権"]
    }
row Software = ConcreteRow
    { rowCode = 0x8053
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Software"
        , asNameJa = "ソフトウェア"
        , asLabelJa = "ソフトウェア"
        , asDescription = "Asset: Software (ソフトウェア)"
        , asAliases = ["ソフトウェア"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["ソフトウェア"]
    }
row CashOverShort = ConcreteRow
    { rowCode = 0x8054
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Cash over and short"
        , asNameJa = "現金過不足"
        , asLabelJa = "現金過不足"
        , asDescription = "Asset: Cash over and short (現金過不足), a temporary/suspense account cleared at closing to MiscellaneousIncome/MiscellaneousLoss"
        , asAliases = ["現金過不足"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [SuspenseOrClearingAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = BookkeepingControlClass Assets
        , asemHomeSideSemantics = ContextDependentHomeSide
        , asemReportingEligibility = NotPresented
        }
    , rowJcciAliases = ["現金過不足"]
    }
row AccountsPayable = ConcreteRow
    { rowCode = 0x8055
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Accounts payable"
        , asNameJa = "買掛金"
        , asLabelJa = "買掛金"
        , asDescription = "Liability: Accounts payable (買掛金)"
        , asAliases = ["買掛金", "a/p", "accounts payable"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["〇〇商店", "買掛金"]
    }
row NotesPayable = ConcreteRow
    { rowCode = 0x8056
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Notes payable"
        , asNameJa = "支払手形"
        , asLabelJa = "支払手形"
        , asDescription = "Liability: Notes payable (支払手形)"
        , asAliases = ["支払手形"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["支払手形"]
    }
row ElectronicallyRecordedObligations = ConcreteRow
    { rowCode = 0x8057
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Electronically recorded monetary obligations"
        , asNameJa = "電子記録債務"
        , asLabelJa = "電子記録債務"
        , asDescription = "Liability: Electronically recorded monetary obligations (電子記録債務)"
        , asAliases = ["電子記録債務"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["電子記録債務"]
    }
row NotesLoansPayable = ConcreteRow
    { rowCode = 0x8058
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Loans payable on notes"
        , asNameJa = "手形借入金"
        , asLabelJa = "手形借入金"
        , asDescription = "Liability: Loans payable on notes (手形借入金)"
        , asAliases = ["手形借入金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["借入金", "手形借入金"]
    }
row BankOverdraft = ConcreteRow
    { rowCode = 0x8059
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Bank overdraft"
        , asNameJa = "当座借越"
        , asLabelJa = "当座借越"
        , asDescription = "Liability: Bank overdraft (当座借越)"
        , asAliases = ["当座借越"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["借入金", "当座借越"]
    }
row AdvancesReceived = ConcreteRow
    { rowCode = 0x805A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Advances received"
        , asNameJa = "前受金"
        , asLabelJa = "前受金"
        , asDescription = "Liability: Advances received (前受金)"
        , asAliases = ["前受金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["前受金", "契約負債"]
    }
row UnearnedRevenue = ConcreteRow
    { rowCode = 0x805B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Unearned revenue"
        , asNameJa = "前受収益"
        , asLabelJa = "前受収益"
        , asDescription = "Liability: Unearned revenue (前受収益), deferral account (経過勘定)"
        , asAliases = ["前受収益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["前受収益", "前受地代"]
    }
row AccruedExpenses = ConcreteRow
    { rowCode = 0x805C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Accrued expenses"
        , asNameJa = "未払費用"
        , asLabelJa = "未払費用"
        , asDescription = "Liability: Accrued expenses (未払費用), accrual account (経過勘定)"
        , asAliases = ["未払費用"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未払費用", "未払利息"]
    }
row OtherPayables = ConcreteRow
    { rowCode = 0x805D
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Other payables"
        , asNameJa = "未払金"
        , asLabelJa = "未払金"
        , asDescription = "Liability: Other payables (未払金)"
        , asAliases = ["未払金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未払金"]
    }
row DepositsReceived = ConcreteRow
    { rowCode = 0x805E
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Deposits received"
        , asNameJa = "預り金"
        , asLabelJa = "預り金"
        , asDescription = "Liability: Deposits received (預り金)"
        , asAliases = ["預り金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["預り金", "従業員預り金", "所得税預り金", "住民税預り金", "社会保険料預り金"]
    }
row SuspenseReceipts = ConcreteRow
    { rowCode = 0x805F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Suspense receipts"
        , asNameJa = "仮受金"
        , asLabelJa = "仮受金"
        , asDescription = "Liability: Suspense receipts (仮受金)"
        , asAliases = ["仮受金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [SuspenseOrClearingAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = BookkeepingControlClass Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = ["仮受金"]
    }
row ConsumptionTaxReceived = ConcreteRow
    { rowCode = 0x8060
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Consumption tax received"
        , asNameJa = "仮受消費税"
        , asLabelJa = "仮受消費税"
        , asDescription = "Liability: Consumption tax received (仮受消費税)"
        , asAliases = ["仮受消費税"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["仮受金", "仮受消費税"]
    }
row AccruedConsumptionTax = ConcreteRow
    { rowCode = 0x8061
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Accrued (unpaid) consumption tax"
        , asNameJa = "未払消費税"
        , asLabelJa = "未払消費税"
        , asDescription = "Liability: Accrued (unpaid) consumption tax (未払消費税)"
        , asAliases = ["未払消費税"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未払金", "未払消費税"]
    }
row AccruedCorporateIncomeTaxes = ConcreteRow
    { rowCode = 0x8062
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Accrued (unpaid) corporate income taxes"
        , asNameJa = "未払法人税等"
        , asLabelJa = "未払法人税等"
        , asDescription = "Liability: Accrued (unpaid) corporate income taxes (未払法人税等)"
        , asAliases = ["未払法人税等"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未払法人税等"]
    }
row UnpaidDividends = ConcreteRow
    { rowCode = 0x8063
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Unpaid dividends"
        , asNameJa = "未払配当金"
        , asLabelJa = "未払配当金"
        , asDescription = "Liability: Unpaid dividends (未払配当金)"
        , asAliases = ["未払配当金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未払配当金", "未払株主配当金"]
    }
row AllowanceForDoubtfulAccounts = ConcreteRow
    { rowCode = 0x8064
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = True
        , asFixedCurrent = Current
        , asNameEn = "Allowance for doubtful accounts"
        , asNameJa = "貸倒引当金"
        , asLabelJa = "貸倒引当金"
        , asDescription = "Asset (contra): Allowance for doubtful accounts (貸倒引当金), a credit-balance valuation account (評価勘定) deducted from receivables. Home side is Credit because it is a contra asset (isContra); values stay non-negative and the Hat/Not structure is intact. B/S deduction (net) presentation is the ExchangeAlgebra.IO.Output module's job."
        , asAliases = ["貸倒引当金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount, ContraAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["貸倒引当金"]
    }
row AccumulatedDepreciation = ConcreteRow
    { rowCode = 0x8065
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = True
        , asFixedCurrent = Fixed
        , asNameEn = "Accumulated depreciation"
        , asNameJa = "減価償却累計額"
        , asLabelJa = "減価償却累計額"
        , asDescription = "Asset (contra): Accumulated depreciation (減価償却累計額), a credit-balance valuation account (評価勘定) under the indirect method (間接法), deducted from the related depreciable assets. Home side is Credit because it is a contra asset (isContra). This is the canonical bookkeeping account for accumulated depreciation; the existing 'ReserveForDepreciation' is retained as the legacy SNA/macro-accounting name."
        , asAliases = ["減価償却累計額"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount, ContraAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["建物減価償却累計額", "減価償却累計額", "備品減価償却累計額", "車両運搬具減価償却累計額", "車両減価償却累計額"]
    }
row LegalRetainedEarnings = ConcreteRow
    { rowCode = 0x8066
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Legal (appropriated) retained earnings reserve"
        , asNameJa = "利益準備金"
        , asLabelJa = "利益準備金"
        , asDescription = "Equity: Legal (appropriated) retained earnings reserve (利益準備金)"
        , asAliases = ["利益準備金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["利益準備金"]
    }
row CumulativeTranslationAdjustment = ConcreteRow
    { rowCode = 0x8067
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Cumulative translation adjustment"
        , asNameJa = "為替換算調整勘定; 在外子会社等の外貨建財務諸表の換算差額を計上する OCI/資本の部の項目"
        , asLabelJa = "為替換算調整勘定"
        , asDescription = "Equity: Cumulative translation adjustment (為替換算調整勘定; 在外子会社等の外貨建財務諸表の換算差額を計上する OCI/資本の部の項目)"
        , asAliases = []
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = ConsolidationOnly
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = []
    }
row ProvisionForDoubtfulAccounts = ConcreteRow
    { rowCode = 0x8068
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Provision for doubtful accounts"
        , asNameJa = "貸倒引当金繰入"
        , asLabelJa = "貸倒引当金繰入"
        , asDescription = "Expense: Provision for doubtful accounts (貸倒引当金繰入)"
        , asAliases = ["貸倒引当金繰入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["貸倒引当金繰入", "貸倒引当金繰入額"]
    }
row BadDebtLoss = ConcreteRow
    { rowCode = 0x8069
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Bad debt loss"
        , asNameJa = "貸倒損失"
        , asLabelJa = "貸倒損失"
        , asDescription = "Expense: Bad debt loss (貸倒損失)"
        , asAliases = ["貸倒損失"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["貸倒損失"]
    }
row LossOnSalesOfFixedAssets = ConcreteRow
    { rowCode = 0x806A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on sales of fixed assets"
        , asNameJa = "固定資産売却損"
        , asLabelJa = "固定資産売却損"
        , asDescription = "Expense: Loss on sales of fixed assets (固定資産売却損)"
        , asAliases = ["固定資産売却損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["固定資産売却損", "備品売却損", "建物売却損", "土地売却損"]
    }
row LossOnSalesOfNotesReceivable = ConcreteRow
    { rowCode = 0x806B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on sales of notes receivable"
        , asNameJa = "手形売却損"
        , asLabelJa = "手形売却損"
        , asDescription = "Expense: Loss on sales of notes receivable (手形売却損)"
        , asAliases = ["手形売却損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["手形売却損"]
    }
row PaymentFees = ConcreteRow
    { rowCode = 0x806C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Payment fees / fees paid"
        , asNameJa = "支払手数料"
        , asLabelJa = "支払手数料"
        , asDescription = "Expense: Payment fees / fees paid (支払手数料), the debit counterpart of 'ReceiptFee'"
        , asAliases = ["支払手数料"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["支払手数料", "販売手数料"]
    }
row MiscellaneousLoss = ConcreteRow
    { rowCode = 0x806D
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Miscellaneous loss"
        , asNameJa = "雑損"
        , asLabelJa = "雑損"
        , asDescription = "Expense: Miscellaneous loss (雑損)"
        , asAliases = ["雑損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["雑損", "雑損失"]
    }
row CorporateIncomeTaxes = ConcreteRow
    { rowCode = 0x806E
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Corporate income taxes"
        , asNameJa = "法人税等"
        , asLabelJa = "法人税等"
        , asDescription = "Expense: Corporate income taxes (法人税等)"
        , asAliases = ["法人税等"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["法人税、住民税及び事業税", "法人税等"]
    }
row CommunicationExpenses = ConcreteRow
    { rowCode = 0x806F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Communication expenses"
        , asNameJa = "通信費"
        , asLabelJa = "通信費"
        , asDescription = "Expense: Communication expenses (通信費)"
        , asAliases = ["通信費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["通信費"]
    }
row GainOnSalesOfFixedAssets = ConcreteRow
    { rowCode = 0x8070
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on sales of fixed assets"
        , asNameJa = "固定資産売却益"
        , asLabelJa = "固定資産売却益"
        , asDescription = "Revenue: Gain on sales of fixed assets (固定資産売却益)"
        , asAliases = ["固定資産売却益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["固定資産売却益", "備品売却益", "土地売却益", "建物売却益"]
    }
row RecoveryOfBadDebts = ConcreteRow
    { rowCode = 0x8071
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Recovery of bad debts written off"
        , asNameJa = "償却債権取立益"
        , asLabelJa = "償却債権取立益"
        , asDescription = "Revenue: Recovery of bad debts written off (償却債権取立益)"
        , asAliases = ["償却債権取立益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["償却債権取立益"]
    }
row MiscellaneousIncome = ConcreteRow
    { rowCode = 0x8072
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Miscellaneous income"
        , asNameJa = "雑益"
        , asLabelJa = "雑益"
        , asDescription = "Revenue: Miscellaneous income (雑益)"
        , asAliases = ["雑益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["雑益", "雑収入", "雑収益"]
    }
row ReversalOfAllowanceForDoubtfulAccounts = ConcreteRow
    { rowCode = 0x8073
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Reversal of allowance for doubtful accounts"
        , asNameJa = "貸倒引当金戻入"
        , asLabelJa = "貸倒引当金戻入"
        , asDescription = "Revenue: Reversal of allowance for doubtful accounts (貸倒引当金戻入). Credit counterpart used by the 差額補充法/洗替法 when the estimated allowance is smaller than the existing balance (the excess of 'AllowanceForDoubtfulAccounts' is released)."
        , asAliases = ["貸倒引当金戻入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["貸倒引当金戻入", "貸倒引当金戻入益"]
    }
row TimeDeposits = ConcreteRow
    { rowCode = 0x8074
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Time deposits"
        , asNameJa = "定期預金"
        , asLabelJa = "定期預金"
        , asDescription = "Asset: Time deposits (定期預金)"
        , asAliases = ["定期預金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["銀行預金", "定期預金", "定期預金○○銀行"]
    }
row LoansReceivable = ConcreteRow
    { rowCode = 0x8075
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Loans receivable"
        , asNameJa = "貸付金"
        , asLabelJa = "貸付金"
        , asDescription = "Asset: Loans receivable (貸付金)"
        , asAliases = ["貸付金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["貸付金", "従業員貸付金", "役員貸付金"]
    }
row GiftCertificatesReceived = ConcreteRow
    { rowCode = 0x8076
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Gift certificates received"
        , asNameJa = "受取商品券"
        , asLabelJa = "受取商品券"
        , asDescription = "Asset: Gift certificates received (受取商品券)"
        , asAliases = ["受取商品券"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["受取商品券"]
    }
row SecurityDepositsPaid = ConcreteRow
    { rowCode = 0x8077
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Guarantee deposits"
        , asNameJa = "差入保証金"
        , asLabelJa = "差入保証金"
        , asDescription = "Asset: Guarantee deposits (差入保証金)"
        , asAliases = ["差入保証金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["差入保証金"]
    }
row SuppliesOnHand = ConcreteRow
    { rowCode = 0x8078
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Supplies"
        , asNameJa = "貯蔵品"
        , asLabelJa = "貯蔵品"
        , asDescription = "Asset: Supplies (貯蔵品)"
        , asAliases = ["貯蔵品"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["貯蔵品"]
    }
row ContractAssets = ConcreteRow
    { rowCode = 0x8079
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Contract assets"
        , asNameJa = "契約資産"
        , asLabelJa = "契約資産"
        , asDescription = "Asset: Contract assets (契約資産)"
        , asAliases = ["契約資産"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["契約資産"]
    }
row IncomeTaxesRefundReceivable = ConcreteRow
    { rowCode = 0x807A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Income taxes refund receivable"
        , asNameJa = "未収還付法人税等"
        , asLabelJa = "未収還付法人税等"
        , asDescription = "Asset: Income taxes refund receivable (未収還付法人税等)"
        , asAliases = ["未収還付法人税等"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未収還付法人税等"]
    }
row WorkInProcess = ConcreteRow
    { rowCode = 0x807B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Work in process"
        , asNameJa = "仕掛品"
        , asLabelJa = "仕掛品"
        , asDescription = "Asset: Work in process (仕掛品)"
        , asAliases = ["仕掛品"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["仕掛品"]
    }
row DeferredTaxAssets = ConcreteRow
    { rowCode = 0x807C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Deferred tax assets"
        , asNameJa = "繰延税金資産"
        , asLabelJa = "繰延税金資産"
        , asDescription = "Asset: Deferred tax assets (繰延税金資産)"
        , asAliases = ["繰延税金資産"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["繰延税金資産"]
    }
row LeasedAssets = ConcreteRow
    { rowCode = 0x807D
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Leased assets"
        , asNameJa = "リース資産"
        , asLabelJa = "リース資産"
        , asDescription = "Asset: Leased assets (リース資産)"
        , asAliases = ["リース資産"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["リース資産"]
    }
row ToolsAndInstruments = ConcreteRow
    { rowCode = 0x807E
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Tools and instruments"
        , asNameJa = "工具器具"
        , asLabelJa = "工具器具"
        , asDescription = "Asset: Tools and instruments (工具器具)"
        , asAliases = ["工具器具"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["工具器具"]
    }
row ConstructionInProgress = ConcreteRow
    { rowCode = 0x807F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Construction in progress"
        , asNameJa = "建設仮勘定"
        , asLabelJa = "建設仮勘定"
        , asDescription = "Asset: Construction in progress (建設仮勘定)"
        , asAliases = ["建設仮勘定"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["建設仮勘定", "建設仮", "建設前渡金", "建設仮払金"]
    }
row Goodwill = ConcreteRow
    { rowCode = 0x8080
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Goodwill"
        , asNameJa = "のれん"
        , asLabelJa = "のれん"
        , asDescription = "Asset: Goodwill (のれん)"
        , asAliases = ["のれん"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["のれん"]
    }
row SoftwareInProgress = ConcreteRow
    { rowCode = 0x8081
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Software in progress"
        , asNameJa = "ソフトウェア仮勘定"
        , asLabelJa = "ソフトウェア仮勘定"
        , asDescription = "Asset: Software in progress (ソフトウェア仮勘定)"
        , asAliases = ["ソフトウェア仮勘定"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["ソフトウェア仮勘定"]
    }
row LongTermPrepaidExpenses = ConcreteRow
    { rowCode = 0x8082
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Long-term prepaid expenses"
        , asNameJa = "長期前払費用"
        , asLabelJa = "長期前払費用"
        , asDescription = "Asset: Long-term prepaid expenses (長期前払費用)"
        , asAliases = ["長期前払費用"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["長期前払費用"]
    }
row DishonoredNotesReceivable = ConcreteRow
    { rowCode = 0x8083
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Dishonored notes receivable"
        , asNameJa = "不渡手形"
        , asLabelJa = "不渡手形"
        , asDescription = "Asset: Dishonored notes receivable (不渡手形)"
        , asAliases = ["不渡手形"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["不渡手形"]
    }
row PrepaidPensionCost = ConcreteRow
    { rowCode = 0x8084
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Prepaid pension costs"
        , asNameJa = "前払年金費用"
        , asLabelJa = "前払年金費用"
        , asDescription = "Asset: Prepaid pension costs (前払年金費用)"
        , asAliases = ["前払年金費用"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["前払年金費用"]
    }
row NetDefinedBenefitAsset = ConcreteRow
    { rowCode = 0x8085
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Retirement benefit asset"
        , asNameJa = "退職給付に係る資産"
        , asLabelJa = "退職給付に係る資産"
        , asDescription = "Asset: Retirement benefit asset (退職給付に係る資産)"
        , asAliases = ["退職給付に係る資産"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["退職給付に係る資産"]
    }
row DepositsInSpecialAccounts = ConcreteRow
    { rowCode = 0x8086
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Deposits in special accounts"
        , asNameJa = "別段預金"
        , asLabelJa = "別段預金"
        , asDescription = "Asset: Deposits in special accounts (別段預金)"
        , asAliases = ["別段預金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["銀行預金", "別段預金"]
    }
row Structures = ConcreteRow
    { rowCode = 0x8087
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Structures"
        , asNameJa = "構築物"
        , asLabelJa = "構築物"
        , asDescription = "Asset: Structures (構築物)"
        , asAliases = ["構築物"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["構築物"]
    }
row LeaseholdRights = ConcreteRow
    { rowCode = 0x8088
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Leasehold interests in land"
        , asNameJa = "借地権"
        , asLabelJa = "借地権"
        , asDescription = "Asset: Leasehold interests in land (借地権)"
        , asAliases = ["借地権"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["借地権"]
    }
row NonOperatingNotesReceivable = ConcreteRow
    { rowCode = 0x8089
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Non-operating notes receivable"
        , asNameJa = "営業外受取手形"
        , asLabelJa = "営業外受取手形"
        , asDescription = "Asset: Non-operating notes receivable (営業外受取手形)"
        , asAliases = ["営業外受取手形"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["営業外受取手形"]
    }
row NonOperatingElectronicallyRecordedReceivable = ConcreteRow
    { rowCode = 0x808A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Electronically recorded monetary claims - non-operating"
        , asNameJa = "営業外電子記録債権"
        , asLabelJa = "営業外電子記録債権"
        , asDescription = "Asset: Electronically recorded monetary claims - non-operating (営業外電子記録債権)"
        , asAliases = ["営業外電子記録債権"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["営業外電子記録債権"]
    }
row RefundLiabilities = ConcreteRow
    { rowCode = 0x808B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Refund liabilities"
        , asNameJa = "返金負債"
        , asLabelJa = "返金負債"
        , asDescription = "Liability: Refund liabilities (返金負債)"
        , asAliases = ["返金負債"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["返金負債"]
    }
row NonOperatingNotesPayable = ConcreteRow
    { rowCode = 0x808C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Non-operating notes payable"
        , asNameJa = "営業外支払手形"
        , asLabelJa = "営業外支払手形"
        , asDescription = "Liability: Non-operating notes payable (営業外支払手形)"
        , asAliases = ["営業外支払手形"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["営業外支払手形"]
    }
row NonOperatingElectronicallyRecordedObligations = ConcreteRow
    { rowCode = 0x808D
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Electronically recorded obligations - non-operating"
        , asNameJa = "営業外電子記録債務"
        , asLabelJa = "営業外電子記録債務"
        , asDescription = "Liability: Electronically recorded obligations - non-operating (営業外電子記録債務)"
        , asAliases = ["営業外電子記録債務"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["営業外電子記録債務"]
    }
row BonusesPayable = ConcreteRow
    { rowCode = 0x808E
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Accrued bonuses"
        , asNameJa = "未払賞与"
        , asLabelJa = "未払賞与"
        , asDescription = "Liability: Accrued bonuses (未払賞与)"
        , asAliases = ["未払賞与"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未払賞与", "未払役員賞与", "未払賞与金"]
    }
row AllowanceForRepairs = ConcreteRow
    { rowCode = 0x808F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Provision for repairs"
        , asNameJa = "修繕引当金"
        , asLabelJa = "修繕引当金"
        , asDescription = "Liability: Provision for repairs (修繕引当金)"
        , asAliases = ["修繕引当金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["修繕引当金", "特別修繕引当金"]
    }
row AllowanceForProductWarranties = ConcreteRow
    { rowCode = 0x8090
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Provision for warranties"
        , asNameJa = "商品保証引当金"
        , asLabelJa = "商品保証引当金"
        , asDescription = "Liability: Provision for warranties (商品保証引当金)"
        , asAliases = ["商品保証引当金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["商品保証引当金", "製品保証引当金"]
    }
row AllowanceForBonuses = ConcreteRow
    { rowCode = 0x8091
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Provision for bonuses"
        , asNameJa = "賞与引当金"
        , asLabelJa = "賞与引当金"
        , asDescription = "Liability: Provision for bonuses (賞与引当金)"
        , asAliases = ["賞与引当金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["賞与引当金", "役員賞与引当金"]
    }
row DeferredTaxLiabilities = ConcreteRow
    { rowCode = 0x8092
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Deferred tax liabilities"
        , asNameJa = "繰延税金負債"
        , asLabelJa = "繰延税金負債"
        , asDescription = "Liability: Deferred tax liabilities (繰延税金負債)"
        , asAliases = ["繰延税金負債"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["繰延税金負債"]
    }
row LeaseObligations = ConcreteRow
    { rowCode = 0x8093
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Lease liabilities"
        , asNameJa = "リース債務"
        , asLabelJa = "リース債務"
        , asDescription = "Liability: Lease liabilities (リース債務)"
        , asAliases = ["リース債務"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["リース債務"]
    }
row GuaranteeDepositsReceived = ConcreteRow
    { rowCode = 0x8094
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Guarantee deposits received"
        , asNameJa = "預り保証金"
        , asLabelJa = "預り保証金"
        , asDescription = "Liability: Guarantee deposits received (預り保証金)"
        , asAliases = ["預り保証金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["預り保証金", "受入保証金"]
    }
row AllowanceForRetirementBenefits = ConcreteRow
    { rowCode = 0x8095
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Provision for retirement benefits"
        , asNameJa = "退職給付引当金"
        , asLabelJa = "退職給付引当金"
        , asDescription = "Liability: Provision for retirement benefits (退職給付引当金)"
        , asAliases = ["退職給付引当金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["退職給付引当金"]
    }
row LongTermOtherPayables = ConcreteRow
    { rowCode = 0x8096
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Long-term accounts payable - other"
        , asNameJa = "長期未払金"
        , asLabelJa = "長期未払金"
        , asDescription = "Liability: Long-term accounts payable - other (長期未払金)"
        , asAliases = ["長期未払金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["長期未払金"]
    }
row NetDefinedBenefitLiability = ConcreteRow
    { rowCode = 0x8097
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Retirement benefit liability"
        , asNameJa = "退職給付に係る負債"
        , asLabelJa = "退職給付に係る負債"
        , asDescription = "Liability: Retirement benefit liability (退職給付に係る負債)"
        , asAliases = ["退職給付に係る負債"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["退職給付に係る負債"]
    }
row StockSubscriptionDeposits = ConcreteRow
    { rowCode = 0x8098
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Deposits for subscriptions of shares"
        , asNameJa = "株式申込証拠金"
        , asLabelJa = "株式申込証拠金"
        , asDescription = "Equity: Deposits for subscriptions of shares (株式申込証拠金)"
        , asAliases = ["株式申込証拠金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["株式申込証拠金", "申込証拠金", "新株式申込証拠金"]
    }
row LegalCapitalSurplus = ConcreteRow
    { rowCode = 0x8099
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Legal capital surplus"
        , asNameJa = "資本準備金"
        , asLabelJa = "資本準備金"
        , asDescription = "Equity: Legal capital surplus (資本準備金)"
        , asAliases = ["資本準備金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["資本準備金", "株式払込剰余金"]
    }
row OtherCapitalSurplus = ConcreteRow
    { rowCode = 0x809A
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Other capital surplus"
        , asNameJa = "その他資本剰余金"
        , asLabelJa = "その他資本剰余金"
        , asDescription = "Equity: Other capital surplus (その他資本剰余金)"
        , asAliases = ["その他資本剰余金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["その他資本剰余金"]
    }
row DividendEqualizationReserve = ConcreteRow
    { rowCode = 0x809B
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Reserve for dividend equalization"
        , asNameJa = "配当平均積立金"
        , asLabelJa = "配当平均積立金"
        , asDescription = "Equity: Reserve for dividend equalization (配当平均積立金)"
        , asAliases = ["配当平均積立金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["配当平均積立金"]
    }
row RepairFundReserve = ConcreteRow
    { rowCode = 0x809C
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Reserve for repairs"
        , asNameJa = "修繕積立金"
        , asLabelJa = "修繕積立金"
        , asDescription = "Equity: Reserve for repairs (修繕積立金)"
        , asAliases = ["修繕積立金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["修繕積立金"]
    }
row ConstructionFundReserve = ConcreteRow
    { rowCode = 0x809D
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Reserve for new construction"
        , asNameJa = "新築積立金"
        , asLabelJa = "新築積立金"
        , asDescription = "Equity: Reserve for new construction (新築積立金)"
        , asAliases = ["新築積立金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["新築積立金"]
    }
row GeneralReserve = ConcreteRow
    { rowCode = 0x809E
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "General reserve"
        , asNameJa = "別途積立金"
        , asLabelJa = "別途積立金"
        , asDescription = "Equity: General reserve (別途積立金)"
        , asAliases = ["別途積立金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["別途積立金"]
    }
row ValuationDifferenceOnOtherSecurities = ConcreteRow
    { rowCode = 0x809F
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Valuation difference on available-for-sale securities"
        , asNameJa = "その他有価証券評価差額金"
        , asLabelJa = "その他有価証券評価差額金"
        , asDescription = "Equity: Valuation difference on available-for-sale securities (その他有価証券評価差額金)"
        , asAliases = ["その他有価証券評価差額金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["その他有価証券評価差額金"]
    }
row NonControllingInterests = ConcreteRow
    { rowCode = 0x80A0
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Non-controlling interests"
        , asNameJa = "非支配株主持分"
        , asLabelJa = "非支配株主持分"
        , asDescription = "Equity: Non-controlling interests (非支配株主持分)"
        , asAliases = ["非支配株主持分"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [AttributionAccount]
        , asemPostingCapability = ConsolidationOnly
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = ["非支配株主持分"]
    }
row CapitalSurplus = ConcreteRow
    { rowCode = 0x80A1
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Capital surplus"
        , asNameJa = "資本剰余金"
        , asLabelJa = "資本剰余金"
        , asDescription = "Equity: Capital surplus (資本剰余金)"
        , asAliases = ["資本剰余金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["資本剰余金"]
    }
row EarnedSurplus = ConcreteRow
    { rowCode = 0x80A2
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Equity
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Retained earnings"
        , asNameJa = "利益剰余金"
        , asLabelJa = "利益剰余金"
        , asDescription = "Equity: Retained earnings (利益剰余金)"
        , asAliases = ["利益剰余金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Equity
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["利益剰余金"]
    }
row ServiceRevenue = ConcreteRow
    { rowCode = 0x80A3
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Service revenue"
        , asNameJa = "役務収益"
        , asLabelJa = "役務収益"
        , asDescription = "Revenue: Service revenue (役務収益)"
        , asAliases = ["役務収益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["役務収益", "営業収益"]
    }
row OperatingRevenue = ConcreteRow
    { rowCode = 0x80A4
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Operating revenue"
        , asNameJa = "営業収益"
        , asLabelJa = "営業収益"
        , asDescription = "Revenue: Operating revenue (営業収益)"
        , asAliases = ["営業収益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["営業収益"]
    }
row GainOnSalesOfSecurities = ConcreteRow
    { rowCode = 0x80A5
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on sale of securities"
        , asNameJa = "有価証券売却益"
        , asLabelJa = "有価証券売却益"
        , asDescription = "Revenue: Gain on sale of securities (有価証券売却益)"
        , asAliases = ["有価証券売却益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["有価証券売却益", "有価証券売買益", "有価証券運用益"]
    }
row GainOnValuationOfSecurities = ConcreteRow
    { rowCode = 0x80A6
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on valuation of securities"
        , asNameJa = "有価証券評価益"
        , asLabelJa = "有価証券評価益"
        , asDescription = "Revenue: Gain on valuation of securities (有価証券評価益)"
        , asAliases = ["有価証券評価益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["有価証券運用益", "有価証券評価益", "有価証券評価損益", "有価証券運用損益"]
    }
row DividendsReceived = ConcreteRow
    { rowCode = 0x80A7
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Dividend income"
        , asNameJa = "受取配当金"
        , asLabelJa = "受取配当金"
        , asDescription = "Revenue: Dividend income (受取配当金)"
        , asAliases = ["受取配当金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["受取配当金"]
    }
row InterestOnSecurities = ConcreteRow
    { rowCode = 0x80A8
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Interest on securities"
        , asNameJa = "有価証券利息"
        , asLabelJa = "有価証券利息"
        , asDescription = "Revenue: Interest on securities (有価証券利息)"
        , asAliases = ["有価証券利息"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["有価証券利息", "受取社債利息"]
    }
row GainOnSalesOfInvestmentSecurities = ConcreteRow
    { rowCode = 0x80A9
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on sale of investment securities"
        , asNameJa = "投資有価証券売却益"
        , asLabelJa = "投資有価証券売却益"
        , asDescription = "Revenue: Gain on sale of investment securities (投資有価証券売却益)"
        , asAliases = ["投資有価証券売却益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["投資有価証券売却益", "その他有価証券売却益"]
    }
row InsuranceGain = ConcreteRow
    { rowCode = 0x80AA
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on insurance claims"
        , asNameJa = "保険差益"
        , asLabelJa = "保険差益"
        , asDescription = "Revenue: Gain on insurance claims (保険差益)"
        , asAliases = ["保険差益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["保険差益"]
    }
row GainOnBargainPurchase = ConcreteRow
    { rowCode = 0x80AB
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on bargain purchase"
        , asNameJa = "負ののれん発生益"
        , asLabelJa = "負ののれん発生益"
        , asDescription = "Revenue: Gain on bargain purchase (負ののれん発生益)"
        , asAliases = ["負ののれん発生益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["負ののれん発生益"]
    }
row ReversalOfAllowanceForRepairs = ConcreteRow
    { rowCode = 0x80AC
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Reversal of provision for repairs"
        , asNameJa = "修繕引当金戻入"
        , asLabelJa = "修繕引当金戻入"
        , asDescription = "Revenue: Reversal of provision for repairs (修繕引当金戻入)"
        , asAliases = ["修繕引当金戻入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["修繕引当金戻入"]
    }
row ReversalOfAllowanceForProductWarranties = ConcreteRow
    { rowCode = 0x80AD
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Reversal of provision for warranties"
        , asNameJa = "商品保証引当金戻入"
        , asLabelJa = "商品保証引当金戻入"
        , asDescription = "Revenue: Reversal of provision for warranties (商品保証引当金戻入)"
        , asAliases = ["商品保証引当金戻入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["商品保証引当金戻入", "製品保証引当金戻入"]
    }
row GainOnDonationOfFixedAssets = ConcreteRow
    { rowCode = 0x80AE
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on receipt of donated non-current assets"
        , asNameJa = "固定資産受贈益"
        , asLabelJa = "固定資産受贈益"
        , asDescription = "Revenue: Gain on receipt of donated non-current assets (固定資産受贈益)"
        , asAliases = ["固定資産受贈益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["固定資産受贈益"]
    }
row GainOnNationalSubsidies = ConcreteRow
    { rowCode = 0x80AF
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on receipt of national subsidies"
        , asNameJa = "国庫補助金受贈益"
        , asLabelJa = "国庫補助金受贈益"
        , asDescription = "Revenue: Gain on receipt of national subsidies (国庫補助金受贈益)"
        , asAliases = ["国庫補助金受贈益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["国庫補助金受贈益"]
    }
row GainOnConstructionGrants = ConcreteRow
    { rowCode = 0x80B0
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Gain on contribution received for construction"
        , asNameJa = "工事負担金受贈益"
        , asLabelJa = "工事負担金受贈益"
        , asDescription = "Revenue: Gain on contribution received for construction (工事負担金受贈益)"
        , asAliases = ["工事負担金受贈益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["工事負担金受贈益"]
    }
row LandRentReceived = ConcreteRow
    { rowCode = 0x80B1
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Rental income from land"
        , asNameJa = "受取地代"
        , asLabelJa = "受取地代"
        , asDescription = "Revenue: Rental income from land (受取地代)"
        , asAliases = ["受取地代"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["受取地代"]
    }
row SalesRebates = ConcreteRow
    { rowCode = 0x80B2
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = True
        , asFixedCurrent = Other
        , asNameEn = "Sales rebates"
        , asNameJa = "売上割戻"
        , asLabelJa = "売上割戻"
        , asDescription = "Revenue: Sales rebates (売上割戻)"
        , asAliases = ["売上割戻"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount, ContraAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["売上割戻"]
    }
row CostOfServices = ConcreteRow
    { rowCode = 0x80B3
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Cost of services"
        , asNameJa = "役務原価"
        , asLabelJa = "役務原価"
        , asDescription = "Expense: Cost of services (役務原価)"
        , asAliases = ["役務原価"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["役務原価", "営業費用"]
    }
row OperatingExpenses = ConcreteRow
    { rowCode = 0x80B4
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Operating expenses"
        , asNameJa = "営業費用"
        , asLabelJa = "営業費用"
        , asDescription = "Expense: Operating expenses (営業費用)"
        , asAliases = ["営業費用"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["営業費用"]
    }
row InventoryShrinkageLoss = ConcreteRow
    { rowCode = 0x80B5
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Inventory shrinkage loss"
        , asNameJa = "棚卸減耗損"
        , asLabelJa = "棚卸減耗損"
        , asDescription = "Expense: Inventory shrinkage loss (棚卸減耗損)"
        , asAliases = ["棚卸減耗損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["棚卸減耗損", "棚卸減耗費"]
    }
row LossOnValuationOfMerchandise = ConcreteRow
    { rowCode = 0x80B6
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Valuation loss on goods"
        , asNameJa = "商品評価損"
        , asLabelJa = "商品評価損"
        , asDescription = "Expense: Valuation loss on goods (商品評価損)"
        , asAliases = ["商品評価損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["商品評価損", "棚卸評価損"]
    }
row Bonuses = ConcreteRow
    { rowCode = 0x80B7
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Bonuses"
        , asNameJa = "賞与"
        , asLabelJa = "賞与"
        , asDescription = "Expense: Bonuses (賞与)"
        , asAliases = ["賞与"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["賞与", "役員賞与"]
    }
row RetirementBenefitExpenses = ConcreteRow
    { rowCode = 0x80B8
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Retirement benefit expenses"
        , asNameJa = "退職給付費用"
        , asLabelJa = "退職給付費用"
        , asDescription = "Expense: Retirement benefit expenses (退職給付費用)"
        , asAliases = ["退職給付費用"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["退職給付費用", "退職給付引当金繰入", "退職給付引当損"]
    }
row ProvisionForRepairs = ConcreteRow
    { rowCode = 0x80B9
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Provision for repairs"
        , asNameJa = "修繕引当金繰入"
        , asLabelJa = "修繕引当金繰入"
        , asDescription = "Expense: Provision for repairs (修繕引当金繰入)"
        , asAliases = ["修繕引当金繰入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["修繕引当金繰入", "特別修繕引当金繰入"]
    }
row ProvisionForBonuses = ConcreteRow
    { rowCode = 0x80BA
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Provision for bonuses"
        , asNameJa = "賞与引当金繰入"
        , asLabelJa = "賞与引当金繰入"
        , asDescription = "Expense: Provision for bonuses (賞与引当金繰入)"
        , asAliases = ["賞与引当金繰入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["賞与引当金繰入", "役員賞与引当金繰入"]
    }
row ProvisionForProductWarranties = ConcreteRow
    { rowCode = 0x80BB
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Provision for warranties"
        , asNameJa = "商品保証引当金繰入"
        , asLabelJa = "商品保証引当金繰入"
        , asDescription = "Expense: Provision for warranties (商品保証引当金繰入)"
        , asAliases = ["商品保証引当金繰入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["商品保証引当金繰入", "製品保証引当金繰入"]
    }
row ResearchAndDevelopmentExpenses = ConcreteRow
    { rowCode = 0x80BC
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Research and development expenses"
        , asNameJa = "研究開発費"
        , asLabelJa = "研究開発費"
        , asDescription = "Expense: Research and development expenses (研究開発費)"
        , asAliases = ["研究開発費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["研究開発費"]
    }
row AmortizationOfGoodwill = ConcreteRow
    { rowCode = 0x80BD
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Amortization of goodwill"
        , asNameJa = "のれん償却"
        , asLabelJa = "のれん償却"
        , asDescription = "Expense: Amortization of goodwill (のれん償却)"
        , asAliases = ["のれん償却"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["のれん償却"]
    }
row AmortizationOfSoftware = ConcreteRow
    { rowCode = 0x80BE
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Amortization of software"
        , asNameJa = "ソフトウェア償却"
        , asLabelJa = "ソフトウェア償却"
        , asDescription = "Expense: Amortization of software (ソフトウェア償却)"
        , asAliases = ["ソフトウェア償却"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["ソフトウェア償却"]
    }
row AmortizationOfPatents = ConcreteRow
    { rowCode = 0x80BF
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Amortization of patent rights"
        , asNameJa = "特許権償却"
        , asLabelJa = "特許権償却"
        , asDescription = "Expense: Amortization of patent rights (特許権償却)"
        , asAliases = ["特許権償却"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["特許権償却"]
    }
row LeaseExpenses = ConcreteRow
    { rowCode = 0x80C0
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Lease expenses"
        , asNameJa = "支払リース料"
        , asLabelJa = "支払リース料"
        , asDescription = "Expense: Lease expenses (支払リース料)"
        , asAliases = ["支払リース料"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["支払リース料"]
    }
row IncorporationExpenses = ConcreteRow
    { rowCode = 0x80C1
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Organization expenses"
        , asNameJa = "創立費"
        , asLabelJa = "創立費"
        , asDescription = "Expense: Organization expenses (創立費)"
        , asAliases = ["創立費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["創立費"]
    }
row StockIssuanceCosts = ConcreteRow
    { rowCode = 0x80C2
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Share issuance costs"
        , asNameJa = "株式交付費"
        , asLabelJa = "株式交付費"
        , asDescription = "Expense: Share issuance costs (株式交付費)"
        , asAliases = ["株式交付費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["株式交付費"]
    }
row BusinessCommencementExpenses = ConcreteRow
    { rowCode = 0x80C3
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Business commencement expenses"
        , asNameJa = "開業費"
        , asLabelJa = "開業費"
        , asDescription = "Expense: Business commencement expenses (開業費)"
        , asAliases = ["開業費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["開業費"]
    }
row DevelopmentExpenses = ConcreteRow
    { rowCode = 0x80C4
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Development expenses"
        , asNameJa = "開発費"
        , asLabelJa = "開発費"
        , asDescription = "Expense: Development expenses (開発費)"
        , asAliases = ["開発費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["開発費"]
    }
row LossOnSalesOfElectronicallyRecordedReceivables = ConcreteRow
    { rowCode = 0x80C5
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on sale of electronically recorded monetary claims"
        , asNameJa = "電子記録債権売却損"
        , asLabelJa = "電子記録債権売却損"
        , asDescription = "Expense: Loss on sale of electronically recorded monetary claims (電子記録債権売却損)"
        , asAliases = ["電子記録債権売却損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["電子記録債権売却損"]
    }
row LossOnSalesOfReceivables = ConcreteRow
    { rowCode = 0x80C6
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on sale of receivables"
        , asNameJa = "債権売却損"
        , asLabelJa = "債権売却損"
        , asDescription = "Expense: Loss on sale of receivables (債権売却損)"
        , asAliases = ["債権売却損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["債権売却損"]
    }
row LossOnSalesOfSecurities = ConcreteRow
    { rowCode = 0x80C7
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on sale of securities"
        , asNameJa = "有価証券売却損"
        , asLabelJa = "有価証券売却損"
        , asDescription = "Expense: Loss on sale of securities (有価証券売却損)"
        , asAliases = ["有価証券売却損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["有価証券売却損", "有価証券売買損", "有価証券運用損"]
    }
row LossOnValuationOfSecurities = ConcreteRow
    { rowCode = 0x80C8
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on valuation of securities"
        , asNameJa = "有価証券評価損"
        , asLabelJa = "有価証券評価損"
        , asDescription = "Expense: Loss on valuation of securities (有価証券評価損)"
        , asAliases = ["有価証券評価損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["有価証券運用損", "有価証券評価損", "有価証券評価損益", "有価証券運用損益"]
    }
row LossOnSalesOfInvestmentSecurities = ConcreteRow
    { rowCode = 0x80C9
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on sale of investment securities"
        , asNameJa = "投資有価証券売却損"
        , asLabelJa = "投資有価証券売却損"
        , asDescription = "Expense: Loss on sale of investment securities (投資有価証券売却損)"
        , asAliases = ["投資有価証券売却損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["投資有価証券売却損", "その他有価証券売却損"]
    }
row LossOnFire = ConcreteRow
    { rowCode = 0x80CA
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on fire"
        , asNameJa = "火災損失"
        , asLabelJa = "火災損失"
        , asDescription = "Expense: Loss on fire (火災損失)"
        , asAliases = ["火災損失"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["火災損失", "災害損失"]
    }
row LossOnRetirementOfFixedAssets = ConcreteRow
    { rowCode = 0x80CB
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on retirement of non-current assets"
        , asNameJa = "固定資産除却損"
        , asLabelJa = "固定資産除却損"
        , asDescription = "Expense: Loss on retirement of non-current assets (固定資産除却損)"
        , asAliases = ["固定資産除却損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["固定資産除却損", "固定資産廃棄損", "備品除却損", "建物除却損", "除却損"]
    }
row LossOnReductionOfFixedAssets = ConcreteRow
    { rowCode = 0x80CC
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss on tax purpose reduction entry of non-current assets"
        , asNameJa = "固定資産圧縮損"
        , asLabelJa = "固定資産圧縮損"
        , asDescription = "Expense: Loss on tax purpose reduction entry of non-current assets (固定資産圧縮損)"
        , asAliases = ["固定資産圧縮損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["固定資産圧縮損", "備品圧縮損", "建物圧縮損", "機械装置圧縮損"]
    }
row AdditionalIncomeTaxesForPriorPeriods = ConcreteRow
    { rowCode = 0x80CD
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Additional income taxes for prior periods"
        , asNameJa = "追徴法人税等"
        , asLabelJa = "追徴法人税等"
        , asDescription = "Expense: Additional income taxes for prior periods (追徴法人税等)"
        , asAliases = ["追徴法人税等"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["追徴法人税等"]
    }
row RefundOfIncomeTaxes = ConcreteRow
    { rowCode = 0x80CE
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = True
        , asFixedCurrent = Other
        , asNameEn = "Refund of income taxes"
        , asNameJa = "還付法人税等"
        , asLabelJa = "還付法人税等"
        , asDescription = "Expense: Refund of income taxes (還付法人税等)"
        , asAliases = ["還付法人税等"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount, ContraAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["還付法人税等"]
    }
row PurchaseRebates = ConcreteRow
    { rowCode = 0x80CF
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = True
        , asFixedCurrent = Other
        , asNameEn = "Purchase rebates"
        , asNameJa = "仕入割戻"
        , asLabelJa = "仕入割戻"
        , asDescription = "Expense: Purchase rebates (仕入割戻)"
        , asAliases = ["仕入割戻"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount, ContraAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["仕入割戻"]
    }
row WelfareExpenses = ConcreteRow
    { rowCode = 0x80D0
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Welfare expenses"
        , asNameJa = "福利厚生費"
        , asLabelJa = "福利厚生費"
        , asDescription = "Expense: Welfare expenses (福利厚生費)"
        , asAliases = ["福利厚生費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["福利厚生費"]
    }
row MaintenanceExpenses = ConcreteRow
    { rowCode = 0x80D1
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Maintenance expenses"
        , asNameJa = "保守費"
        , asLabelJa = "保守費"
        , asDescription = "Expense: Maintenance expenses (保守費)"
        , asAliases = ["保守費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["保守費", "維持費", "支払メンテナンス料"]
    }
row StatutoryWelfareExpenses = ConcreteRow
    { rowCode = 0x80D2
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Legal welfare expenses"
        , asNameJa = "法定福利費"
        , asLabelJa = "法定福利費"
        , asDescription = "Expense: Legal welfare expenses (法定福利費)"
        , asAliases = ["法定福利費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["法定福利費", "社会保険料"]
    }
row LandRentPaid = ConcreteRow
    { rowCode = 0x80D3
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Rent expenses on land"
        , asNameJa = "支払地代"
        , asLabelJa = "支払地代"
        , asDescription = "Expense: Rent expenses on land (支払地代)"
        , asAliases = ["支払地代"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["地代家賃", "支払賃借料", "支払不動産賃借料", "支払地代"]
    }
row InsuranceExpense = ConcreteRow
    { rowCode = 0x80D4
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Insurance expenses"
        , asNameJa = "保険料"
        , asLabelJa = "保険料"
        , asDescription = "Expense: Insurance expenses (保険料)"
        , asAliases = ["保険料"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["保険料", "支払保険料", "火災保険料"]
    }
row RepairsExpense = ConcreteRow
    { rowCode = 0x80D5
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Repair expenses"
        , asNameJa = "修繕費"
        , asLabelJa = "修繕費"
        , asDescription = "Expense: Repair expenses (修繕費)"
        , asAliases = ["修繕費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["修繕費", "支払修繕料", "修理費"]
    }
row StorageExpenses = ConcreteRow
    { rowCode = 0x80D6
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Storage costs"
        , asNameJa = "保管費"
        , asLabelJa = "保管費"
        , asDescription = "Expense: Storage costs (保管費)"
        , asAliases = ["保管費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["保管費", "保管料", "倉庫料"]
    }
row MembershipFees = ConcreteRow
    { rowCode = 0x80D7
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Membership fees"
        , asNameJa = "諸会費"
        , asLabelJa = "諸会費"
        , asDescription = "Expense: Membership fees (諸会費)"
        , asAliases = ["諸会費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["諸会費"]
    }
row IncomeSummary = ConcreteRow
    { rowCode = 0x80D8
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = NoClose
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Income summary"
        , asNameJa = "損益"
        , asLabelJa = "損益"
        , asDescription = "Bookkeeping device: Income summary (損益). Assets/NoClose is a technical debit-side placeholder; it is not a balance-sheet asset and is not automatically closed."
        , asAliases = ["損益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [ClosingDevice]
        , asemPostingCapability = ClosingOnly
        , asemDivisionSemantics = DirectionEncoding Assets
        , asemHomeSideSemantics = ContextDependentHomeSide
        , asemReportingEligibility = NotPresented
        }
    , rowJcciAliases = ["損益"]
    }
row SuspenseAccount = ConcreteRow
    { rowCode = 0x80D9
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Suspense account"
        , asNameJa = "未決算"
        , asLabelJa = "未決算"
        , asDescription = "Asset: Suspense account (未決算)"
        , asAliases = ["未決算"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [SuspenseOrClearingAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = BookkeepingControlClass Assets
        , asemHomeSideSemantics = ContextDependentHomeSide
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = ["未決算", "火災未決算", "保険未決算"]
    }
row ForeignExchangeGains = ConcreteRow
    { rowCode = 0x80DA
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Foreign exchange gains"
        , asNameJa = "為替差益"
        , asLabelJa = "為替差益"
        , asDescription = "Revenue: Foreign exchange gains (為替差益)"
        , asAliases = ["為替差益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["為替差損益", "為替差益"]
    }
row ForeignExchangeLosses = ConcreteRow
    { rowCode = 0x80DB
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Foreign exchange losses"
        , asNameJa = "為替差損"
        , asLabelJa = "為替差損"
        , asDescription = "Expense: Foreign exchange losses (為替差損)"
        , asAliases = ["為替差損"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["為替差損益", "為替差損"]
    }
row ContraAccountForGuaranteeObligations = ConcreteRow
    { rowCode = 0x80DC
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Contra account for guarantee obligations"
        , asNameJa = "保証債務見返"
        , asLabelJa = "保証債務見返"
        , asDescription = "Asset: Contra account for guarantee obligations (保証債務見返)"
        , asAliases = ["保証債務見返"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["保証債務見返"]
    }
row GuaranteeObligations = ConcreteRow
    { rowCode = 0x80DD
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Guarantee obligations"
        , asNameJa = "保証債務"
        , asLabelJa = "保証債務"
        , asDescription = "Liability: Guarantee obligations (保証債務)"
        , asAliases = ["保証債務"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["保証債務"]
    }
row IncomeTaxesAdjustment = ConcreteRow
    { rowCode = 0x80DE
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Income taxes - deferred"
        , asNameJa = "法人税等調整額"
        , asLabelJa = "法人税等調整額"
        , asDescription = "Expense: Income taxes - deferred (法人税等調整額)"
        , asAliases = ["法人税等調整額"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["法人税等調整額"]
    }
row BranchCurrentAccount = ConcreteRow
    { rowCode = 0x80DF
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Branch current account"
        , asNameJa = "支店"
        , asLabelJa = "支店"
        , asDescription = "Asset: Branch current account (支店)"
        , asAliases = ["支店"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [ReciprocalAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = BookkeepingControlClass Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = ["支店"]
    }
row HeadOfficeCurrentAccount = ConcreteRow
    { rowCode = 0x80E0
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Head office current account"
        , asNameJa = "本店"
        , asLabelJa = "本店"
        , asDescription = "Liability: Head office current account (本店), the credit-balance reciprocal account in branch books."
        , asAliases = ["本店"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [ReciprocalAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = BookkeepingControlClass Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = ["本店"]
    }
row NetIncomeAttributableToNCI = ConcreteRow
    { rowCode = 0x80E1
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = NoClose
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Profit attributable to non-controlling interests"
        , asNameJa = "非支配株主に帰属する当期純利益"
        , asLabelJa = "非支配株主に帰属する当期純利益"
        , asDescription = "Expense: Profit attributable to non-controlling interests (非支配株主に帰属する当期純利益). NoClose: consolidation procedures transfer it separately to non-controlling interests."
        , asAliases = ["非支配株主に帰属する当期純利益"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [AttributionAccount, PeriodResult]
        , asemPostingCapability = ConsolidationOnly
        , asemDivisionSemantics = DirectionEncoding Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = ["非支配株主に帰属する当期純利益"]
    }
row NetLossAttributableToNCI = ConcreteRow
    { rowCode = 0x80E2
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = NoClose
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Loss attributable to non-controlling interests"
        , asNameJa = "非支配株主に帰属する当期純損失"
        , asLabelJa = "非支配株主に帰属する当期純損失"
        , asDescription = "Revenue: Loss attributable to non-controlling interests (非支配株主に帰属する当期純損失). NoClose: consolidation procedures transfer it separately to non-controlling interests."
        , asAliases = ["非支配株主に帰属する当期純損失"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [AttributionAccount, PeriodResult]
        , asemPostingCapability = ConsolidationOnly
        , asemDivisionSemantics = DirectionEncoding Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = ContextualPresentation
        }
    , rowJcciAliases = ["非支配株主に帰属する当期純損失"]
    }
row TradingSecurities = ConcreteRow
    { rowCode = 0x80E3
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Trading securities"
        , asNameJa = "売買目的有価証券"
        , asLabelJa = "売買目的有価証券"
        , asDescription = "Asset: Trading securities (売買目的有価証券)"
        , asAliases = ["売買目的有価証券"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["売買目的有価証券", "有価証券"]
    }
row HeldToMaturityBonds = ConcreteRow
    { rowCode = 0x80E4
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Held-to-maturity bonds"
        , asNameJa = "満期保有目的債券"
        , asLabelJa = "満期保有目的債券"
        , asDescription = "Asset: Held-to-maturity bonds (満期保有目的債券)"
        , asAliases = ["満期保有目的債券"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["満期保有目的債券", "投資有価証券"]
    }
row SubsidiaryStocks = ConcreteRow
    { rowCode = 0x80E5
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Shares of subsidiaries"
        , asNameJa = "子会社株式"
        , asLabelJa = "子会社株式"
        , asDescription = "Asset: Shares of subsidiaries (子会社株式)"
        , asAliases = ["子会社株式"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["子会社株式", "関係会社株式"]
    }
row AffiliateStocks = ConcreteRow
    { rowCode = 0x80E6
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Shares of associates"
        , asNameJa = "関連会社株式"
        , asLabelJa = "関連会社株式"
        , asDescription = "Asset: Shares of associates (関連会社株式)"
        , asAliases = ["関連会社株式"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["関係会社株式", "関連会社株式"]
    }
row AvailableForSaleSecurities = ConcreteRow
    { rowCode = 0x80E7
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Fixed
        , asNameEn = "Available-for-sale securities"
        , asNameJa = "その他有価証券"
        , asLabelJa = "その他有価証券"
        , asDescription = "Asset: Available-for-sale securities (その他有価証券)"
        , asAliases = ["その他有価証券"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["投資有価証券", "その他有価証券"]
    }
row ConsumptionTaxRefundReceivable = ConcreteRow
    { rowCode = 0x80E8
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Consumption tax refund receivable"
        , asNameJa = "未収還付消費税"
        , asLabelJa = "未収還付消費税"
        , asDescription = "Asset: Consumption tax refund receivable (未収還付消費税)"
        , asAliases = ["未収還付消費税", "未収消費税"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未収還付消費税", "未収消費税"]
    }
row PropertyTaxPayable = ConcreteRow
    { rowCode = 0x80E9
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Property tax payable"
        , asNameJa = "未払固定資産税"
        , asLabelJa = "未払固定資産税"
        , asDescription = "Liability: Property tax payable (未払固定資産税)"
        , asAliases = ["未払固定資産税"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["未払固定資産税"]
    }
row DepositsReceivedFromOfficers = ConcreteRow
    { rowCode = 0x80EA
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Liability
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Deposits received from officers"
        , asNameJa = "役員預り金"
        , asLabelJa = "役員預り金"
        , asDescription = "Liability: Deposits received from officers (役員預り金)"
        , asAliases = ["役員預り金"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Liability
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = ["役員預り金"]
    }
row EntertainmentExpenses = ConcreteRow
    { rowCode = 0x80EB
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Entertainment expenses"
        , asNameJa = "交際費"
        , asLabelJa = "交際費"
        , asDescription = "Expense: Entertainment expenses (交際費)"
        , asAliases = ["交際費", "接待交際費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row MeetingExpenses = ConcreteRow
    { rowCode = 0x80EC
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Meeting expenses"
        , asNameJa = "会議費"
        , asLabelJa = "会議費"
        , asDescription = "Expense: Meeting expenses (会議費)"
        , asAliases = ["会議費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row NewspaperBooksExpenses = ConcreteRow
    { rowCode = 0x80ED
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Newspapers and books"
        , asNameJa = "新聞図書費"
        , asLabelJa = "新聞図書費"
        , asDescription = "Expense: Newspapers and books (新聞図書費)"
        , asAliases = ["新聞図書費"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row RawMaterials = ConcreteRow
    { rowCode = 0x80EE
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Raw materials"
        , asNameJa = "原材料"
        , asLabelJa = "原材料"
        , asDescription = "Asset: Raw materials (原材料)"
        , asAliases = ["原材料", "材料"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row GoodsInTransit = ConcreteRow
    { rowCode = 0x80EF
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Assets
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Current
        , asNameEn = "Goods in transit"
        , asNameJa = "未着品"
        , asLabelJa = "未着品"
        , asDescription = "Asset: Goods in transit (未着品)"
        , asAliases = ["未着品"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Assets
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row SalesAllowances = ConcreteRow
    { rowCode = 0x80F0
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = True
        , asFixedCurrent = Other
        , asNameEn = "Sales allowances"
        , asNameJa = "売上値引"
        , asLabelJa = "売上値引"
        , asDescription = "Revenue: Sales allowances (売上値引)"
        , asAliases = ["売上値引"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount, ContraAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row PurchaseAllowances = ConcreteRow
    { rowCode = 0x80F1
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = True
        , asFixedCurrent = Other
        , asNameEn = "Purchase allowances"
        , asNameJa = "仕入値引"
        , asLabelJa = "仕入値引"
        , asDescription = "Expense: Purchase allowances (仕入値引)"
        , asAliases = ["仕入値引"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount, ContraAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row ReversalOfAllowanceForBonuses = ConcreteRow
    { rowCode = 0x80F2
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Revenue
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Reversal of provision for bonuses"
        , asNameJa = "賞与引当金戻入"
        , asLabelJa = "賞与引当金戻入"
        , asDescription = "Revenue: Reversal of provision for bonuses (賞与引当金戻入)"
        , asAliases = ["賞与引当金戻入"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Revenue
        , asemHomeSideSemantics = FixedHomeSide Credit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row DirectorsCompensations = ConcreteRow
    { rowCode = 0x80F3
    , rowScheme = Jcci
    , rowSpec = AccountSpec
        { asDivision = Cost
        , asClosing = CloseByDivision
        , asIsContra = False
        , asFixedCurrent = Other
        , asNameEn = "Remuneration for directors (and other officers)"
        , asNameJa = "役員報酬"
        , asLabelJa = "役員報酬"
        , asDescription = "Expense: Remuneration for directors (役員報酬)"
        , asAliases = ["役員報酬"]
        }
    , rowSemantics = AccountSemantics
        { asemRoles = [OrdinaryAccount]
        , asemPostingCapability = OrdinaryPosting
        , asemDivisionSemantics = StatementDivision Cost
        , asemHomeSideSemantics = FixedHomeSide Debit
        , asemReportingEligibility = StatementEligible
        }
    , rowJcciAliases = []
    }
row AccountTitle = WildcardRow

instance NFData AccountTitles

instance Hashable AccountTitles where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt x = hashWithSalt salt (fromEnum x)

-- | Serialize a title as a two-byte big-endian code: @0x8000 + n@, where
-- @n@ is the title's fixed number from 'accountCode', or @0xFFFF@ for the
-- wildcard 'AccountTitle'. The high bit marks the 0.6 encoding: bytes written
-- by 0.5.x (the declaration index, always below @0x8000@) and unknown codes
-- fail to decode, so a title added later never shifts an existing code.
instance Binary.Binary AccountTitles where
    {-# INLINE put #-}
    put = BinaryPut.putWord16be . accountCode
    {-# INLINE get #-}
    get = do
        code <- BinaryGet.getWord16be
        case Map.lookup code accountTitlesByCode of
            Just title -> pure title
            Nothing    -> fail ("AccountTitles: unknown or unmarked code " ++ show code)

-- | The fixed two-byte wire code of a title.
accountCode :: AccountTitles -> Word16
accountCode title = case row title of
    WildcardRow                    -> 0xFFFF
    ConcreteRow { rowCode = code } -> code

-- | Inverse of the fixed codes, shared by every decode.
accountTitlesByCode :: Map.Map Word16 AccountTitles
accountTitlesByCode = Map.fromList [(accountCode title, title) | title <- [minBound .. maxBound]]

instance Element AccountTitles where
    {-# INLINE wildcard #-}
    wildcard = AccountTitle

instance BaseClass AccountTitles where
