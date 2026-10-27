{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE DeriveGeneric              #-}

module ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..)) where

import ExchangeAlgebra.Algebra.Element.Representation (Element(..))
import ExchangeAlgebra.Algebra.Base.Representation (BaseClass(..))
import GHC.Generics (Generic)
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData(..))
import Data.Word (Word16)
import qualified Data.Map.Strict as Map
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as BinaryGet
import qualified Data.Binary.Put as BinaryPut

-- ** Account Titles

-- | Account titles used as bookkeeping basis coordinates.
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
                    | ConsumptionTaxRefundReceivable -- ^ Assets: Consumption tax refund receivable (未収還付消費税)
                    | PropertyTaxPayable -- ^ Liability: Property tax payable (未払固定資産税)
                    | DepositsReceivedFromOfficers -- ^ Liability: Deposits received from officers (役員預り金)
                    | EntertainmentExpenses -- ^ Cost: Entertainment expenses (交際費)
                    | MeetingExpenses -- ^ Cost: Meeting expenses (会議費)
                    | NewspaperBooksExpenses -- ^ Cost: Newspapers and books (新聞図書費)
                    | RawMaterials -- ^ Assets: Raw materials (原材料)
                    | GoodsInTransit -- ^ Assets: Goods in transit (未着品)
                    | SalesAllowances -- ^ Revenue: Sales allowances (売上値引)
                    | PurchaseAllowances -- ^ Cost: Purchase allowances (仕入値引)
                    | ReversalOfAllowanceForBonuses
                      -- ^ Revenue: Reversal of provision for bonuses (賞与引当金戻入)
                    | AccountTitle                    -- ^ Wildcard (ワイルドカード — 任意の科目にマッチ。projWithBase 等の問い合わせ用で, 実 posting には使わない)
                    deriving (Show, Ord, Eq, Enum, Generic, Bounded)

-- Commutation の実用法は examples の文脈から「通信費」であることが確認済
-- (2026-06-11 調査)。Enum/Binary 序数の安定のため削除はせず, 新名称への移行を促す。
{-# DEPRECATED Commutation "通信費 (communication expenses) — use 'CommunicationExpenses' instead" #-}

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

-- | The fixed wire code of each title. The case is total and written out, so
-- adding a title without a code fails to compile. The numbers are the
-- declaration indices of develop at this change, frozen; a new title
-- takes the next unused number wherever it is declared. Decoding uses the
-- inverse table built from this function, not a second hand-written table.
accountCode :: AccountTitles -> Word16
accountCode title = case title of
    Cash                                           -> 0x8000
    Deposits                                       -> 0x8001
    CurrentDeposits                                -> 0x8002
    Securities                                     -> 0x8003
    InvestmentSecurities                           -> 0x8004
    InvestmentInAssociate                          -> 0x8005
    LongTermNationalBonds                          -> 0x8006
    ShortTermNationalBonds                         -> 0x8007
    Products                                       -> 0x8008
    Machinery                                      -> 0x8009
    Building                                       -> 0x800A
    Vehicle                                        -> 0x800B
    StockInvestment                                -> 0x800C
    EquipmentInvestment                            -> 0x800D
    LongTermLoansReceivable                        -> 0x800E
    AccountsReceivable                             -> 0x800F
    ShortTermLoansReceivable                       -> 0x8010
    ReserveDepositReceivable                       -> 0x8011
    Gold                                           -> 0x8012
    GovernmentService                              -> 0x8013
    CapitalStock                                   -> 0x8014
    RetainedEarnings                               -> 0x8015
    LongTermLoansPayable                           -> 0x8016
    ShortTermLoansPayable                          -> 0x8017
    LoansPayable                                   -> 0x8018
    ReserveForDepreciation                         -> 0x8019
    DepositPayable                                 -> 0x801A
    LongTermNationalBondsPayable                   -> 0x801B
    ShortTermNationalBondsPayable                  -> 0x801C
    ReserveDepositPayable                          -> 0x801D
    CentralBankNotePayable                         -> 0x801E
    Depreciation                                   -> 0x801F
    AmortizationExpense                            -> 0x8020
    SalesCost                                      -> 0x8021
    BusinessTrip                                   -> 0x8022
    Commutation                                    -> 0x8023
    UtilitiesExpense                               -> 0x8024
    RentExpense                                    -> 0x8025
    AdvertisingExpense                             -> 0x8026
    DeliveryExpenses                               -> 0x8027
    SuppliesExpenses                               -> 0x8028
    MiscellaneousExpenses                          -> 0x8029
    WageExpenditure                                -> 0x802A
    InterestExpense                                -> 0x802B
    TaxesExpense                                   -> 0x802C
    ConsumptionExpenditure                         -> 0x802D
    SubsidyExpense                                 -> 0x802E
    CentralBankPaymentExpense                      -> 0x802F
    Purchases                                      -> 0x8030
    NetIncome                                      -> 0x8031
    ValueAdded                                     -> 0x8032
    SubsidyIncome                                  -> 0x8033
    NationalBondInterestEarned                     -> 0x8034
    DepositInterestEarned                          -> 0x8035
    GrossProfit                                    -> 0x8036
    OrdinaryProfit                                 -> 0x8037
    InterestEarned                                 -> 0x8038
    ReceiptFee                                     -> 0x8039
    RentalIncome                                   -> 0x803A
    WageEarned                                     -> 0x803B
    TaxesRevenue                                   -> 0x803C
    CentralBankPaymentIncome                       -> 0x803D
    Sales                                          -> 0x803E
    EquityInEarningsOfInvestee                     -> 0x803F
    NetLoss                                        -> 0x8040
    PettyCash                                      -> 0x8041
    NotesReceivable                                -> 0x8042
    ElectronicallyRecordedReceivable               -> 0x8043
    CreditCardReceivable                           -> 0x8044
    NotesLoansReceivable                           -> 0x8045
    MerchandiseInventory                           -> 0x8046
    AdvancesPaid                                   -> 0x8047
    PrepaidExpenses                                -> 0x8048
    AccruedRevenue                                 -> 0x8049
    OtherReceivables                               -> 0x804A
    PaymentsOnBehalf                               -> 0x804B
    SuspensePayments                               -> 0x804C
    ConsumptionTaxPaid                             -> 0x804D
    PrepaidCorporateIncomeTaxes                    -> 0x804E
    Land                                           -> 0x804F
    Fixtures                                       -> 0x8050
    Patent                                         -> 0x8051
    Trademark                                      -> 0x8052
    Software                                       -> 0x8053
    CashOverShort                                  -> 0x8054
    AccountsPayable                                -> 0x8055
    NotesPayable                                   -> 0x8056
    ElectronicallyRecordedObligations              -> 0x8057
    NotesLoansPayable                              -> 0x8058
    BankOverdraft                                  -> 0x8059
    AdvancesReceived                               -> 0x805A
    UnearnedRevenue                                -> 0x805B
    AccruedExpenses                                -> 0x805C
    OtherPayables                                  -> 0x805D
    DepositsReceived                               -> 0x805E
    SuspenseReceipts                               -> 0x805F
    ConsumptionTaxReceived                         -> 0x8060
    AccruedConsumptionTax                          -> 0x8061
    AccruedCorporateIncomeTaxes                    -> 0x8062
    UnpaidDividends                                -> 0x8063
    AllowanceForDoubtfulAccounts                   -> 0x8064
    AccumulatedDepreciation                        -> 0x8065
    LegalRetainedEarnings                          -> 0x8066
    CumulativeTranslationAdjustment                -> 0x8067
    ProvisionForDoubtfulAccounts                   -> 0x8068
    BadDebtLoss                                    -> 0x8069
    LossOnSalesOfFixedAssets                       -> 0x806A
    LossOnSalesOfNotesReceivable                   -> 0x806B
    PaymentFees                                    -> 0x806C
    MiscellaneousLoss                              -> 0x806D
    CorporateIncomeTaxes                           -> 0x806E
    CommunicationExpenses                          -> 0x806F
    GainOnSalesOfFixedAssets                       -> 0x8070
    RecoveryOfBadDebts                             -> 0x8071
    MiscellaneousIncome                            -> 0x8072
    ReversalOfAllowanceForDoubtfulAccounts         -> 0x8073
    TimeDeposits                                   -> 0x8074
    LoansReceivable                                -> 0x8075
    GiftCertificatesReceived                       -> 0x8076
    SecurityDepositsPaid                           -> 0x8077
    SuppliesOnHand                                 -> 0x8078
    ContractAssets                                 -> 0x8079
    IncomeTaxesRefundReceivable                    -> 0x807A
    WorkInProcess                                  -> 0x807B
    DeferredTaxAssets                              -> 0x807C
    LeasedAssets                                   -> 0x807D
    ToolsAndInstruments                            -> 0x807E
    ConstructionInProgress                         -> 0x807F
    Goodwill                                       -> 0x8080
    SoftwareInProgress                             -> 0x8081
    LongTermPrepaidExpenses                        -> 0x8082
    DishonoredNotesReceivable                      -> 0x8083
    PrepaidPensionCost                             -> 0x8084
    NetDefinedBenefitAsset                         -> 0x8085
    DepositsInSpecialAccounts                      -> 0x8086
    Structures                                     -> 0x8087
    LeaseholdRights                                -> 0x8088
    NonOperatingNotesReceivable                    -> 0x8089
    NonOperatingElectronicallyRecordedReceivable   -> 0x808A
    RefundLiabilities                              -> 0x808B
    NonOperatingNotesPayable                       -> 0x808C
    NonOperatingElectronicallyRecordedObligations  -> 0x808D
    BonusesPayable                                 -> 0x808E
    AllowanceForRepairs                            -> 0x808F
    AllowanceForProductWarranties                  -> 0x8090
    AllowanceForBonuses                            -> 0x8091
    DeferredTaxLiabilities                         -> 0x8092
    LeaseObligations                               -> 0x8093
    GuaranteeDepositsReceived                      -> 0x8094
    AllowanceForRetirementBenefits                 -> 0x8095
    LongTermOtherPayables                          -> 0x8096
    NetDefinedBenefitLiability                     -> 0x8097
    StockSubscriptionDeposits                      -> 0x8098
    LegalCapitalSurplus                            -> 0x8099
    OtherCapitalSurplus                            -> 0x809A
    DividendEqualizationReserve                    -> 0x809B
    RepairFundReserve                              -> 0x809C
    ConstructionFundReserve                        -> 0x809D
    GeneralReserve                                 -> 0x809E
    ValuationDifferenceOnOtherSecurities           -> 0x809F
    NonControllingInterests                        -> 0x80A0
    CapitalSurplus                                 -> 0x80A1
    EarnedSurplus                                  -> 0x80A2
    ServiceRevenue                                 -> 0x80A3
    OperatingRevenue                               -> 0x80A4
    GainOnSalesOfSecurities                        -> 0x80A5
    GainOnValuationOfSecurities                    -> 0x80A6
    DividendsReceived                              -> 0x80A7
    InterestOnSecurities                           -> 0x80A8
    GainOnSalesOfInvestmentSecurities              -> 0x80A9
    InsuranceGain                                  -> 0x80AA
    GainOnBargainPurchase                          -> 0x80AB
    ReversalOfAllowanceForRepairs                  -> 0x80AC
    ReversalOfAllowanceForProductWarranties        -> 0x80AD
    GainOnDonationOfFixedAssets                    -> 0x80AE
    GainOnNationalSubsidies                        -> 0x80AF
    GainOnConstructionGrants                       -> 0x80B0
    LandRentReceived                               -> 0x80B1
    SalesRebates                                   -> 0x80B2
    CostOfServices                                 -> 0x80B3
    OperatingExpenses                              -> 0x80B4
    InventoryShrinkageLoss                         -> 0x80B5
    LossOnValuationOfMerchandise                   -> 0x80B6
    Bonuses                                        -> 0x80B7
    RetirementBenefitExpenses                      -> 0x80B8
    ProvisionForRepairs                            -> 0x80B9
    ProvisionForBonuses                            -> 0x80BA
    ProvisionForProductWarranties                  -> 0x80BB
    ResearchAndDevelopmentExpenses                 -> 0x80BC
    AmortizationOfGoodwill                         -> 0x80BD
    AmortizationOfSoftware                         -> 0x80BE
    AmortizationOfPatents                          -> 0x80BF
    LeaseExpenses                                  -> 0x80C0
    IncorporationExpenses                          -> 0x80C1
    StockIssuanceCosts                             -> 0x80C2
    BusinessCommencementExpenses                   -> 0x80C3
    DevelopmentExpenses                            -> 0x80C4
    LossOnSalesOfElectronicallyRecordedReceivables -> 0x80C5
    LossOnSalesOfReceivables                       -> 0x80C6
    LossOnSalesOfSecurities                        -> 0x80C7
    LossOnValuationOfSecurities                    -> 0x80C8
    LossOnSalesOfInvestmentSecurities              -> 0x80C9
    LossOnFire                                     -> 0x80CA
    LossOnRetirementOfFixedAssets                  -> 0x80CB
    LossOnReductionOfFixedAssets                   -> 0x80CC
    AdditionalIncomeTaxesForPriorPeriods           -> 0x80CD
    RefundOfIncomeTaxes                            -> 0x80CE
    PurchaseRebates                                -> 0x80CF
    WelfareExpenses                                -> 0x80D0
    MaintenanceExpenses                            -> 0x80D1
    StatutoryWelfareExpenses                       -> 0x80D2
    LandRentPaid                                   -> 0x80D3
    InsuranceExpense                               -> 0x80D4
    RepairsExpense                                 -> 0x80D5
    StorageExpenses                                -> 0x80D6
    MembershipFees                                 -> 0x80D7
    IncomeSummary                                  -> 0x80D8
    SuspenseAccount                                -> 0x80D9
    ForeignExchangeGains                           -> 0x80DA
    ForeignExchangeLosses                          -> 0x80DB
    ContraAccountForGuaranteeObligations           -> 0x80DC
    GuaranteeObligations                           -> 0x80DD
    IncomeTaxesAdjustment                          -> 0x80DE
    BranchCurrentAccount                           -> 0x80DF
    HeadOfficeCurrentAccount                       -> 0x80E0
    NetIncomeAttributableToNCI                     -> 0x80E1
    NetLossAttributableToNCI                       -> 0x80E2
    TradingSecurities                              -> 0x80E3
    HeldToMaturityBonds                            -> 0x80E4
    SubsidiaryStocks                               -> 0x80E5
    AffiliateStocks                                -> 0x80E6
    AvailableForSaleSecurities                     -> 0x80E7
    ConsumptionTaxRefundReceivable                 -> 0x80E8
    PropertyTaxPayable                             -> 0x80E9
    DepositsReceivedFromOfficers                   -> 0x80EA
    EntertainmentExpenses                          -> 0x80EB
    MeetingExpenses                                -> 0x80EC
    NewspaperBooksExpenses                         -> 0x80ED
    RawMaterials                                   -> 0x80EE
    GoodsInTransit                                 -> 0x80EF
    SalesAllowances                                -> 0x80F0
    PurchaseAllowances                             -> 0x80F1
    ReversalOfAllowanceForBonuses                  -> 0x80F2
    AccountTitle                                   -> 0xFFFF

-- | Inverse of the fixed codes, shared by every decode.
accountTitlesByCode :: Map.Map Word16 AccountTitles
accountTitlesByCode = Map.fromList [(accountCode title, title) | title <- [minBound .. maxBound]]

instance Element AccountTitles where
    {-# INLINE wildcard #-}
    wildcard = AccountTitle




instance BaseClass AccountTitles where

