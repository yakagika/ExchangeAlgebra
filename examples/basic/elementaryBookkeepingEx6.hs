{-
  ebex6 — 現金預金・商品売買・債権債務
  レクチャー初級簿記 第 9-15 章対応 (数値は変換済; 桁感のみ踏襲)

  扱う論点:
    第  9 章 現金過不足 (発生→原因判明→決算で雑益/雑損), 小口現金, 当座借越
    第 10 章 3 分法の商品売買 (仕入/売上/繰越商品), 返品, 前払金/前受金
    第 12 章 クレジット売掛金 (支払手数料)
    第 14 章 約束手形 (受取/支払), 電子記録債権/債務, (手形) 貸付/借入
    第 15 章 立替金/預り金/仮払金/仮受金/未収入金/未払金

  構築は原則スマートコンストラクタ '.@' を使う (非負・Zero 正規化を一箇所で強制)。
  集約は bar/norm を明示的に呼ぶ (暗黙の bar/compress は使わない)。
-}

import qualified ExchangeAlgebra            as EA
import           ExchangeAlgebra
import           System.Exit                (exitFailure)
import           Control.Monad              (unless)

-- | 勘定科目だけの交換代数基底 (ebex1 と同じ単純形)
type MinBase = EA.HatBase EA.AccountTitles

-- | 取引情報
type MinTransaction = EA.Alg MoneyDecimal MinBase

------------------------------------------------------------------
-- 第 9 章 現金預金
------------------------------------------------------------------

-- 現金過不足: 帳簿残高より実際有高が ¥3,000 少なかった (不足)。
-- (借) 現金過不足 3,000 (貸) 現金 3,000
ex9_overShortOccur :: MinTransaction
ex9_overShortOccur
    =  3000 .@ Not :< CashOverShort   -- 現金過不足 (仮勘定)
    .+ 3000 .@ Hat :< Cash            -- 現金の減少

-- 後日, うち ¥2,000 は通信費の記帳漏れと判明。
-- (借) 通信費 2,000 (貸) 現金過不足 2,000
ex9_overShortFound :: MinTransaction
ex9_overShortFound
    =  2000 .@ Not :< CommunicationExpenses
    .+ 2000 .@ Hat :< CashOverShort

-- 決算: 原因不明の残額 ¥1,000 を雑損へ振り替える。
-- (借) 雑損 1,000 (貸) 現金過不足 1,000
ex9_overShortClose :: MinTransaction
ex9_overShortClose
    =  1000 .@ Not :< MiscellaneousLoss
    .+ 1000 .@ Hat :< CashOverShort

-- 小口現金: 定額資金前渡制で小口現金 ¥30,000 を当座預金から前渡し。
-- (借) 小口現金 30,000 (貸) 当座預金 30,000
ex9_pettyCashAdvance :: MinTransaction
ex9_pettyCashAdvance
    =  30000 .@ Not :< PettyCash
    .+ 30000 .@ Hat :< CurrentDeposits

-- 小口現金から旅費 ¥8,000・通信費 ¥5,000 を支払った旨の報告を受け補給。
-- (借) 旅費交通費 8,000 / 通信費 5,000 (貸) 当座預金 13,000
ex9_pettyCashReplenish :: MinTransaction
ex9_pettyCashReplenish
    =  8000 .@ Not :< BusinessTrip
    .+ 5000 .@ Not :< CommunicationExpenses
    .+ 13000 .@ Hat :< CurrentDeposits

-- 当座借越: 当座預金残高 ¥40,000 のところ ¥70,000 の小切手を振り出した。
-- 不足 ¥30,000 は当座借越 (負債) となる。
-- (借) 仕入 70,000 (貸) 当座預金 40,000 / 当座借越 30,000
ex9_overdraft :: MinTransaction
ex9_overdraft
    =  70000 .@ Not :< Purchases
    .+ 40000 .@ Hat :< CurrentDeposits
    .+ 30000 .@ Not :< BankOverdraft

------------------------------------------------------------------
-- 第 10 章 商品売買 (3 分法) + 前払金/前受金
------------------------------------------------------------------

-- 仕入: 商品 ¥120,000 を掛けで仕入れた。
-- (借) 仕入 120,000 (貸) 買掛金 120,000
ex10_purchase :: MinTransaction
ex10_purchase
    =  120000 .@ Not :< Purchases
    .+ 120000 .@ Not :< AccountsPayable

-- 仕入戻し (返品): 上記のうち ¥20,000 を品違いで返品した。
-- (借) 買掛金 20,000 (貸) 仕入 20,000
ex10_purchaseReturn :: MinTransaction
ex10_purchaseReturn
    =  20000 .@ Hat :< AccountsPayable
    .+ 20000 .@ Hat :< Purchases

-- 売上: 商品を ¥200,000 で掛け売りした。
-- (借) 売掛金 200,000 (貸) 売上 200,000
ex10_sale :: MinTransaction
ex10_sale
    =  200000 .@ Not :< AccountsReceivable
    .+ 200000 .@ Not :< Sales

-- 売上戻り (返品): 上記のうち ¥30,000 が返品された。
-- (借) 売上 30,000 (貸) 売掛金 30,000
ex10_salesReturn :: MinTransaction
ex10_salesReturn
    =  30000 .@ Hat :< Sales
    .+ 30000 .@ Hat :< AccountsReceivable

-- 前払金: 商品注文に先立ち手付金 ¥15,000 を現金で支払った。
-- (借) 前払金 15,000 (貸) 現金 15,000
ex10_advancePaid :: MinTransaction
ex10_advancePaid
    =  15000 .@ Not :< AdvancesPaid
    .+ 15000 .@ Hat :< Cash

-- 前受金: 商品注文を受け手付金 ¥25,000 を現金で受け取った。
-- (借) 現金 25,000 (貸) 前受金 25,000
ex10_advanceReceived :: MinTransaction
ex10_advanceReceived
    =  25000 .@ Not :< Cash
    .+ 25000 .@ Not :< AdvancesReceived

------------------------------------------------------------------
-- 第 12 章 クレジット売掛金
------------------------------------------------------------------

-- クレジット払いで商品 ¥100,000 を売り上げ。信販会社への手数料 4% (¥4,000) は
-- 販売時に支払手数料として計上する。
-- (借) クレジット売掛金 96,000 / 支払手数料 4,000 (貸) 売上 100,000
ex12_creditSale :: MinTransaction
ex12_creditSale
    =  96000 .@ Not :< CreditCardReceivable
    .+ 4000  .@ Not :< PaymentFees
    .+ 100000 .@ Not :< Sales

-- 後日, 信販会社から手数料控除後の ¥96,000 が当座預金に入金。
-- (借) 当座預金 96,000 (貸) クレジット売掛金 96,000
ex12_creditCollect :: MinTransaction
ex12_creditCollect
    =  96000 .@ Not :< CurrentDeposits
    .+ 96000 .@ Hat :< CreditCardReceivable

------------------------------------------------------------------
-- 第 14 章 約束手形・電子記録債権債務・手形貸付借入
------------------------------------------------------------------

-- 受取手形: 商品 ¥80,000 を売り上げ, 代金は約束手形で受け取った。
-- (借) 受取手形 80,000 (貸) 売上 80,000
ex14_notesReceivable :: MinTransaction
ex14_notesReceivable
    =  80000 .@ Not :< NotesReceivable
    .+ 80000 .@ Not :< Sales

-- 支払手形: 商品 ¥60,000 を仕入れ, 代金は約束手形を振り出した。
-- (借) 仕入 60,000 (貸) 支払手形 60,000
ex14_notesPayable :: MinTransaction
ex14_notesPayable
    =  60000 .@ Not :< Purchases
    .+ 60000 .@ Not :< NotesPayable

-- 電子記録債権: 売掛金 ¥50,000 について電子記録債権の発生記録を行った。
-- (借) 電子記録債権 50,000 (貸) 売掛金 50,000
ex14_erReceivable :: MinTransaction
ex14_erReceivable
    =  50000 .@ Not :< ElectronicallyRecordedReceivable
    .+ 50000 .@ Hat :< AccountsReceivable

-- 電子記録債務: 買掛金 ¥40,000 について電子記録債務の発生記録を行った。
-- (借) 買掛金 40,000 (貸) 電子記録債務 40,000
ex14_erObligation :: MinTransaction
ex14_erObligation
    =  40000 .@ Hat :< AccountsPayable
    .+ 40000 .@ Not :< ElectronicallyRecordedObligations

-- 手形貸付金: 取引先に ¥150,000 を貸し付け, 約束手形を受け取った。
-- (借) 手形貸付金 150,000 (貸) 現金 150,000
ex14_notesLoanGiven :: MinTransaction
ex14_notesLoanGiven
    =  150000 .@ Not :< NotesLoansReceivable
    .+ 150000 .@ Hat :< Cash

-- 手形借入金: 銀行から ¥200,000 を借り入れ, 約束手形を振り出した。
-- (借) 当座預金 200,000 (貸) 手形借入金 200,000
ex14_notesLoanTaken :: MinTransaction
ex14_notesLoanTaken
    =  200000 .@ Not :< CurrentDeposits
    .+ 200000 .@ Not :< NotesLoansPayable

------------------------------------------------------------------
-- 第 15 章 その他の債権債務
------------------------------------------------------------------

-- 立替金: 従業員が負担すべき金額 ¥7,000 を会社が現金で立替払いした。
-- (借) 立替金 7,000 (貸) 現金 7,000
ex15_paymentOnBehalf :: MinTransaction
ex15_paymentOnBehalf
    =  7000 .@ Not :< PaymentsOnBehalf
    .+ 7000 .@ Hat :< Cash

-- 預り金: 給料 ¥180,000 の支払時, 源泉所得税 ¥12,000 を預かり差引現金支給。
-- (借) 給料 180,000 (貸) 預り金 12,000 / 現金 168,000
ex15_depositsReceived :: MinTransaction
ex15_depositsReceived
    =  180000 .@ Not :< WageExpenditure
    .+ 12000  .@ Not :< DepositsReceived
    .+ 168000 .@ Hat :< Cash

-- 仮払金: 従業員の出張に際し概算額 ¥40,000 を現金で渡した。
-- (借) 仮払金 40,000 (貸) 現金 40,000
ex15_suspensePayment :: MinTransaction
ex15_suspensePayment
    =  40000 .@ Not :< SuspensePayments
    .+ 40000 .@ Hat :< Cash

-- 仮払金の精算: 旅費 ¥35,000 と判明し, 残額 ¥5,000 を現金で受け取った。
-- (借) 旅費交通費 35,000 / 現金 5,000 (貸) 仮払金 40,000
ex15_suspensePaymentSettle :: MinTransaction
ex15_suspensePaymentSettle
    =  35000 .@ Not :< BusinessTrip
    .+ 5000  .@ Not :< Cash
    .+ 40000 .@ Hat :< SuspensePayments

-- 仮受金: 内容不明の入金 ¥60,000 が当座預金にあった。
-- (借) 当座預金 60,000 (貸) 仮受金 60,000
ex15_suspenseReceipt :: MinTransaction
ex15_suspenseReceipt
    =  60000 .@ Not :< CurrentDeposits
    .+ 60000 .@ Not :< SuspenseReceipts

-- 仮受金の精算: 上記は売掛金の回収と判明。
-- (借) 仮受金 60,000 (貸) 売掛金 60,000
ex15_suspenseReceiptSettle :: MinTransaction
ex15_suspenseReceiptSettle
    =  60000 .@ Hat :< SuspenseReceipts
    .+ 60000 .@ Hat :< AccountsReceivable

-- 未収入金: 備品 (簿価 ¥100,000) を ¥100,000 で売却し代金は月末受取とした。
-- (借) 未収入金 100,000 (貸) 備品 100,000
ex15_otherReceivable :: MinTransaction
ex15_otherReceivable
    =  100000 .@ Not :< OtherReceivables
    .+ 100000 .@ Hat :< Fixtures

-- 未払金: 業務用パソコン ¥90,000 を購入し代金は翌月払いとした。
-- (借) 備品 90,000 (貸) 未払金 90,000
ex15_otherPayable :: MinTransaction
ex15_otherPayable
    =  90000 .@ Not :< Fixtures
    .+ 90000 .@ Not :< OtherPayables

------------------------------------------------------------------
-- すべての取引を集計
------------------------------------------------------------------

allEntries :: MinTransaction
allEntries
    =  ex9_overShortOccur .+ ex9_overShortFound .+ ex9_overShortClose
    .+ ex9_pettyCashAdvance .+ ex9_pettyCashReplenish .+ ex9_overdraft
    .+ ex10_purchase .+ ex10_purchaseReturn .+ ex10_sale .+ ex10_salesReturn
    .+ ex10_advancePaid .+ ex10_advanceReceived
    .+ ex12_creditSale .+ ex12_creditCollect
    .+ ex14_notesReceivable .+ ex14_notesPayable
    .+ ex14_erReceivable .+ ex14_erObligation
    .+ ex14_notesLoanGiven .+ ex14_notesLoanTaken
    .+ ex15_paymentOnBehalf .+ ex15_depositsReceived
    .+ ex15_suspensePayment .+ ex15_suspensePaymentSettle
    .+ ex15_suspenseReceipt .+ ex15_suspenseReceiptSettle
    .+ ex15_otherReceivable .+ ex15_otherPayable

-- 各仕訳は借方合計 = 貸方合計 (貸借一致) を満たすはず。
-- decL = 借方側, decR = 貸方側。norm で量を取り出して比較する。
debitTotal, creditTotal :: MoneyDecimal
debitTotal  = norm (decL allEntries)
creditTotal = norm (decR allEntries)

main :: IO ()
main = do
    putStrLn "=== ebex6: 現金預金・商品売買・債権債務 (第 9-15 章) ==="
    putStrLn ""
    putStrLn "全 28 仕訳の集計 (借方合計 / 貸方合計):"
    putStrLn $ "  Debit  total = " ++ show debitTotal
    putStrLn $ "  Credit total = " ++ show creditTotal
    putStrLn ""

    -- 試算表 CSV を出力
    writeCompoundTrialBalance "examples/basic/result/csv/ebex6_trial_balance.csv" allEntries
    putStrLn "wrote: examples/basic/result/csv/ebex6_trial_balance.csv"

    -- 検算: 貸借一致
    unless (debitTotal == creditTotal) $ do
        putStrLn "ASSERTION FAILED: debit total /= credit total"
        exitFailure
    putStrLn ""
    putStrLn "OK: debit total == credit total"
