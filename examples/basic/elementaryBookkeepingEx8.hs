{-
  ebex8 — 資本・訂正仕訳・経過勘定・税
  レクチャー初級簿記 第 19-23 章対応 (数値は変換済; 桁感のみ踏襲)

  扱う論点:
    第 19 章 株式の発行 (増資), 剰余金の配当 (利益準備金積立 + 未払配当金)
    第 20 章 訂正仕訳 = reversingEntry (Hat 対合) で誤仕訳を取り消し正仕訳を追加。
             seq に訂正履歴が残ること (監査証跡) を writeAccountOf で見せる。
    第 21 章 経過勘定 4 種 (前払/前受/未収/未払) + 翌期首の再振替 (reversingEntry)
    第 23 章 消費税 (仮払/仮受 → 確定 consumptionTaxSettlementEntry),
             法人税等 (中間納付 / 確定)

  決算整理の builder は ExchangeAlgebra.Bookkeeping から。
  訂正仕訳の監査証跡を見せる箇所だけ日付つき基底 (AccountTitles, Day) を使う。
-}

import qualified ExchangeAlgebra            as EA
import           ExchangeAlgebra
import           ExchangeAlgebra.Bookkeeping
import           Data.Time
import           System.Exit                (exitFailure)
import           Control.Monad              (unless)

-- 単純基底 (大半の仕訳)
type MinBase        = EA.HatBase EA.AccountTitles
type MinTransaction = EA.Alg MoneyDecimal MinBase

mk :: MkBase MinBase
mk = (:<)

-- 日付つき基底 (訂正仕訳の監査証跡を勘定元帳に出すため)
type ADBase        = EA.HatBase (EA.AccountTitles, Day)
type ADTransaction = EA.Alg MoneyDecimal ADBase

d :: Integer -> Int -> Int -> Day
d = fromGregorian

getDay :: ADBase -> Day
getDay (_ :< (_, dy)) = dy

------------------------------------------------------------------
-- 第 19 章 株式発行・剰余金の配当
------------------------------------------------------------------

-- 増資: 新株 200 株を 1 株 ¥5,000 で発行し, 払込金が当座預金に入金された。
-- (借) 当座預金 1,000,000 (貸) 資本金 1,000,000
ex19_issueStock :: MinTransaction
ex19_issueStock
    =  (200 * 5000) .@ Not :< CurrentDeposits
    .+ (200 * 5000) .@ Not :< CapitalStock

-- 剰余金の配当: 株主総会で繰越利益剰余金から配当 ¥300,000 を決議し,
-- あわせて利益準備金 ¥30,000 を積み立てた。
-- (借) 繰越利益剰余金 330,000
--   (貸) 未払配当金 300,000 / 利益準備金 30,000
ex19_dividend :: MinTransaction
ex19_dividend
    =  330000 .@ Hat :< RetainedEarnings
    .+ 300000 .@ Not :< UnpaidDividends
    .+ 30000  .@ Not :< LegalRetainedEarnings

-- 配当金の支払: 後日, 未払配当金 ¥300,000 を当座預金から支払った。
-- (借) 未払配当金 300,000 (貸) 当座預金 300,000
ex19_payDividend :: MinTransaction
ex19_payDividend
    =  300000 .@ Hat :< UnpaidDividends
    .+ 300000 .@ Hat :< CurrentDeposits

------------------------------------------------------------------
-- 第 20 章 訂正仕訳 (Hat 対合による取消 + 正仕訳)
------------------------------------------------------------------

-- 誤り: 売掛金 ¥45,000 の現金回収を, 誤って「売上の現金受取」として記帳した。
--   (誤) (借) 現金 45,000 (貸) 売上 45,000
-- 正しくは
--   (正) (借) 現金 45,000 (貸) 売掛金 45,000
--
-- 訂正は「誤仕訳の Hat 取消」+「正仕訳」を seq に追加する。
-- reversingEntry = (.^) = Hat 対合。両者を保持することで, 元帳に
-- 「誤記 → 取消 → 正記」の監査証跡がそのまま残る (交換代数の冗長性)。

-- 誤った仕訳 (3/10)
wrongEntry :: ADTransaction
wrongEntry
    =  45000 .@ Not :< (Cash,  d 2024 3 10)
    .+ 45000 .@ Not :< (Sales, d 2024 3 10)

-- 訂正取消: 誤仕訳を Hat 対合 (reversingEntry) でそのまま打ち消す。
-- 元仕訳と同じ基底 (同日付) を反転するので, bar で正味化すると誤記が相殺される。
cancelEntry :: ADTransaction
cancelEntry = reversingEntry wrongEntry

-- 正しい仕訳 (3/15)
correctEntry :: ADTransaction
correctEntry
    =  45000 .@ Not :< (Cash,               d 2024 3 15)
    .+ 45000 .@ Hat :< (AccountsReceivable, d 2024 3 15)

-- 訂正後の台帳全体 (誤記 + 取消 + 正記 を seq に保持する)
correctionLedger :: ADTransaction
correctionLedger = wrongEntry .+ cancelEntry .+ correctEntry

------------------------------------------------------------------
-- 第 21 章 経過勘定 (見越・繰延) + 翌期首の再振替
------------------------------------------------------------------

-- (1) 前払費用: 支払家賃 ¥48,000 のうち翌期分 ¥12,000 を繰り延べる。
ex21_prepaid :: MinTransaction
ex21_prepaid = prepaidExpenseEntry mk 12000 RentExpense

-- (2) 前受収益: 受取家賃 ¥36,000 のうち翌期分 ¥9,000 を繰り延べる。
ex21_unearned :: MinTransaction
ex21_unearned = unearnedRevenueEntry mk 9000 RentalIncome

-- (3) 未収収益: 当期に発生済だが未受取の受取利息 ¥6,000 を見越し計上する。
ex21_accruedRev :: MinTransaction
ex21_accruedRev = accruedRevenueEntry mk 6000 InterestEarned

-- (4) 未払費用: 当期に発生済だが未払の支払利息 ¥7,500 を見越し計上する。
ex21_accruedExp :: MinTransaction
ex21_accruedExp = accruedExpenseEntry mk 7500 InterestExpense

-- 翌期首の再振替: 経過勘定は翌期首に逆仕訳で戻す (reversingEntry)。
-- 4 つの経過勘定すべてを再振替する。
ex21_reversal :: MinTransaction
ex21_reversal = reversingEntry
    (ex21_prepaid .+ ex21_unearned .+ ex21_accruedRev .+ ex21_accruedExp)

------------------------------------------------------------------
-- 第 23 章 消費税・法人税等
------------------------------------------------------------------

-- 期中: 仕入時に仮払消費税 ¥20,000, 売上時に仮受消費税 ¥35,000 を計上済みとする。
-- 決算: 仮受 − 仮払 = ¥15,000 を未払消費税として確定する。
ex23_consumptionTax :: MinTransaction
ex23_consumptionTax = consumptionTaxSettlementEntry mk 20000 35000

-- 法人税等: 中間納付 ¥40,000 を現金で行った (仮払法人税等)。
ex23_corpTaxInterim :: MinTransaction
ex23_corpTaxInterim = corporateTaxInterimEntry mk 40000

-- 決算: 法人税等の年間確定額 ¥90,000。中間 ¥40,000 を控除し未払 ¥50,000 を確定。
ex23_corpTaxSettle :: MinTransaction
ex23_corpTaxSettle = corporateTaxSettlementEntries mk 90000 40000

------------------------------------------------------------------
-- 検算
------------------------------------------------------------------

simpleBalanced :: [(String, MinTransaction)]
simpleBalanced =
    [ ("ex19_issueStock",     ex19_issueStock)
    , ("ex19_dividend",       ex19_dividend)
    , ("ex19_payDividend",    ex19_payDividend)
    , ("ex21_prepaid",        ex21_prepaid)
    , ("ex21_unearned",       ex21_unearned)
    , ("ex21_accruedRev",     ex21_accruedRev)
    , ("ex21_accruedExp",     ex21_accruedExp)
    , ("ex21_reversal",       ex21_reversal)
    , ("ex23_consumptionTax", ex23_consumptionTax)
    , ("ex23_corpTaxInterim", ex23_corpTaxInterim)
    , ("ex23_corpTaxSettle",  ex23_corpTaxSettle)
    ]

main :: IO ()
main = do
    putStrLn "=== ebex8: 資本・訂正仕訳・経過勘定・税 (第 19-23 章) ==="
    putStrLn ""

    -- 訂正仕訳: 監査証跡 (誤記 → 取消 → 正記) を勘定元帳に出力
    putStrLn "--- 訂正仕訳の監査証跡 (第 20 章) ---"
    putStrLn "訂正前 (誤記のみ) の現金・売掛金・売上 元帳:"
    writeAccountOf [Cash, AccountsReceivable, Sales]
                   "examples/basic/result/csv/ebex8_correction_before.csv"
                   wrongEntry getDay
    putStrLn "  wrote: examples/basic/result/csv/ebex8_correction_before.csv"
    putStrLn "訂正後 (誤記 + 取消 + 正記 を seq に保持) の元帳:"
    writeAccountOf [Cash, AccountsReceivable, Sales]
                   "examples/basic/result/csv/ebex8_correction_after.csv"
                   correctionLedger getDay
    putStrLn "  wrote: examples/basic/result/csv/ebex8_correction_after.csv"
    putStrLn ""
    -- seq の冗長性: 訂正後台帳の posting 件数 (集約していないこと) を示す
    putStrLn $ "訂正後台帳の posting 件数 (集約していない) = "
             ++ show (length (EA.toList correctionLedger))
    -- bar で正味化すると 誤記の Sales が取消で相殺され, 正しい売掛金の減少が残る
    putStrLn "bar で正味化すると 誤記の Sales は取消で相殺される:"
    putStrLn $ "  Sales 正味 = "
             ++ show (norm (projByAccountTitle Sales (bar correctionLedger)))
             ++ " (誤記が取消され 0)"
    putStrLn $ "  AccountsReceivable 正味 = "
             ++ show (norm (projByAccountTitle AccountsReceivable (bar correctionLedger)))
             ++ " (正仕訳の売掛金回収分)"
    putStrLn ""

    -- 経過勘定の再振替が元仕訳を打ち消すこと
    putStrLn "--- 経過勘定の再振替 (第 21 章) ---"
    let accruals = ex21_prepaid .+ ex21_unearned .+ ex21_accruedRev .+ ex21_accruedExp
    putStrLn $ "経過勘定 + 翌期首再振替を bar で正味化 = Zero ? "
             ++ show (bar (accruals .+ ex21_reversal) == Zero)
    putStrLn ""

    -- 税の確定額
    putStrLn "--- 税 (第 23 章) ---"
    putStrLn $ "未払消費税 = "
             ++ show (norm (projByAccountTitle AccruedConsumptionTax ex23_consumptionTax))
    putStrLn $ "未払法人税等 = "
             ++ show (norm (projByAccountTitle AccruedCorporateIncomeTaxes ex23_corpTaxSettle))
    putStrLn ""

    -- 試算表 CSV (単純基底の仕訳まとめ)
    let allSimple = foldr (.+) Zero (Prelude.map snd simpleBalanced)
    writeCompoundTrialBalance "examples/basic/result/csv/ebex8_trial_balance.csv" allSimple
    putStrLn "wrote: examples/basic/result/csv/ebex8_trial_balance.csv"
    putStrLn ""

    -- 検算 1: 単純基底の各仕訳が貸借一致
    let badSimple = [ name | (name, e) <- simpleBalanced, norm (decL e) /= norm (decR e) ]
    -- 検算 2: 訂正後台帳も貸借一致 (誤記と取消が相殺された残りも均衡)
    let correctionBalanced = norm (decL correctionLedger) == norm (decR correctionLedger)
    -- 検算 3: 再振替が元の経過勘定を相殺する
    let reversalCancels = bar (accruals .+ ex21_reversal) == Zero
    unless (null badSimple && correctionBalanced && reversalCancels) $ do
        putStrLn $ "ASSERTION FAILED: unbalanced=" ++ show badSimple
                 ++ " correctionBalanced=" ++ show correctionBalanced
                 ++ " reversalCancels=" ++ show reversalCancels
        exitFailure
    putStrLn "OK: all entries balanced; reversal cancels the accruals"
