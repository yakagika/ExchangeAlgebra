{-
  ebex9 — 決算の全体像・財務諸表 (総合例)
  レクチャー初級簿記 第 24-25 章対応 (数値は変換済; 桁感のみ踏襲)

  簿記一巡の総合ショーケース:
    1. 期中取引 (ebex6-8 の科目を横断する 15 仕訳)
    2. 決算整理 (Bookkeeping builder: 売上原価/減価償却/貸倒引当金/経過勘定)
    3. 8 桁精算表 (writeWorksheet) — 決算整理前残高 + 整理仕訳
    4. 決算振替 (incomeSummaryAccount → netIncomeTransfer)
    5. 繰越試算表 (writePostClosingTrialBalance)
    6. B/S・P/L (writeBS / writePL)

  新 API のフルショーケース。構築は '.@', 集約は bar/norm を明示。
-}

import qualified ExchangeAlgebra            as EA
import           ExchangeAlgebra
import           ExchangeAlgebra.Bookkeeping
import qualified Data.Text                  as T
import           System.Exit                (exitFailure)
import           Control.Monad              (unless)

type MinBase        = EA.HatBase EA.AccountTitles
type MinTransaction = EA.Alg MoneyDecimal MinBase

mk :: MkBase MinBase
mk = (:<)

------------------------------------------------------------------
-- 0. 期首残高 (前期繰越)
------------------------------------------------------------------
-- 期首の繰越商品 (3 分法) と貸倒引当金・減価償却累計額の残高。
-- これらは期中の損益には現れないが, 決算整理の基礎になる。

opening :: MinTransaction
opening
    =  500000 .@ Not :< Cash
    .+ 300000 .@ Not :< CurrentDeposits
    .+ 200000 .@ Not :< AccountsReceivable
    .+ 80000  .@ Not :< MerchandiseInventory          -- 期首商品棚卸高
    .+ 600000 .@ Not :< Fixtures                       -- 備品 (取得原価)
    .+ 120000 .@ Not :< AccumulatedDepreciation        -- 減価償却累計額 (評価勘定)
    .+ 4000   .@ Not :< AllowanceForDoubtfulAccounts   -- 貸倒引当金 (評価勘定)
    .+ 150000 .@ Not :< AccountsPayable
    .+ 200000 .@ Not :< LoansPayable
    .+ 1000000 .@ Not :< CapitalStock
    .+ 206000 .@ Not :< RetainedEarnings               -- 貸借差額 (期首繰越利益剰余金)

------------------------------------------------------------------
-- 1. 期中取引 (15 仕訳)
------------------------------------------------------------------

-- (1) 商品 ¥400,000 を掛けで仕入れた。
t01 :: MinTransaction
t01 = 400000 .@ Not :< Purchases .+ 400000 .@ Not :< AccountsPayable

-- (2) 商品 ¥900,000 を掛けで売り上げた。
t02 :: MinTransaction
t02 = 900000 .@ Not :< AccountsReceivable .+ 900000 .@ Not :< Sales

-- (3) 売掛金 ¥500,000 を現金で回収した。
t03 :: MinTransaction
t03 = 500000 .@ Not :< Cash .+ 500000 .@ Hat :< AccountsReceivable

-- (4) 買掛金 ¥350,000 を当座預金から支払った。
t04 :: MinTransaction
t04 = 350000 .@ Hat :< AccountsPayable .+ 350000 .@ Hat :< CurrentDeposits

-- (5) 給料 ¥180,000 を現金で支払った。
t05 :: MinTransaction
t05 = 180000 .@ Not :< WageExpenditure .+ 180000 .@ Hat :< Cash

-- (6) 支払家賃 1 年分 ¥120,000 を現金で支払った (うち翌期分は決算で繰延)。
t06 :: MinTransaction
t06 = 120000 .@ Not :< RentExpense .+ 120000 .@ Hat :< Cash

-- (7) 受取手数料 ¥40,000 を現金で受け取った。
t07 :: MinTransaction
t07 = 40000 .@ Not :< Cash .+ 40000 .@ Not :< ReceiptFee

-- (8) 通信費 ¥15,000 を現金で支払った。
t08 :: MinTransaction
t08 = 15000 .@ Not :< CommunicationExpenses .+ 15000 .@ Hat :< Cash

-- (9) 旅費交通費 ¥25,000 を現金で支払った。
t09 :: MinTransaction
t09 = 25000 .@ Not :< BusinessTrip .+ 25000 .@ Hat :< Cash

-- (10) 借入金の利息 ¥6,000 を現金で支払った。
t10 :: MinTransaction
t10 = 6000 .@ Not :< InterestExpense .+ 6000 .@ Hat :< Cash

-- (11) 備品 ¥100,000 を購入し代金は翌月払い (未払金) とした。
t11 :: MinTransaction
t11 = 100000 .@ Not :< Fixtures .+ 100000 .@ Not :< OtherPayables

-- (12) 売掛金のうち ¥3,000 が貸し倒れた (引当金を充当)。
t12 :: MinTransaction
t12 = 3000 .@ Hat :< AllowanceForDoubtfulAccounts .+ 3000 .@ Hat :< AccountsReceivable

-- (13) 当座預金から現金 ¥100,000 を引き出した。
t13 :: MinTransaction
t13 = 100000 .@ Not :< Cash .+ 100000 .@ Hat :< CurrentDeposits

-- (14) 消耗品 (事務用品) ¥8,000 を現金で購入した。
t14 :: MinTransaction
t14 = 8000 .@ Not :< SuppliesExpenses .+ 8000 .@ Hat :< Cash

-- (15) 水道光熱費 ¥12,000 を現金で支払った。
t15 :: MinTransaction
t15 = 12000 .@ Not :< UtilitiesExpense .+ 12000 .@ Hat :< Cash

periodTransactions :: MinTransaction
periodTransactions
    =  t01 .+ t02 .+ t03 .+ t04 .+ t05 .+ t06 .+ t07 .+ t08
    .+ t09 .+ t10 .+ t11 .+ t12 .+ t13 .+ t14 .+ t15

------------------------------------------------------------------
-- 2. 決算整理前残高 (期首 + 期中)
------------------------------------------------------------------

preAdjustment :: MinTransaction
preAdjustment = opening .+ periodTransactions

------------------------------------------------------------------
-- 3. 決算整理仕訳 (Bookkeeping builder)
------------------------------------------------------------------

-- (a) 売上原価の算定 (3 分法): 期首 ¥80,000, 期末 ¥110,000。
--     しいくりくりしい: 仕入/繰越商品 を振り替える。
adjCOGS :: MinTransaction
adjCOGS = cogsAdjustmentEntries mk 80000 110000

-- (b) 減価償却 (間接法): 備品の当期償却費 ¥70,000。
adjDepreciation :: MinTransaction
adjDepreciation = depreciationIndirectEntry mk 70000

-- (c) 貸倒引当金 (差額補充法): 期末売掛金から見積額 ¥7,000。
--     期末の引当金残高 = 4,000 - 3,000 (t12 充当) = 1,000。差額 6,000 を繰入。
adjAllowance :: MinTransaction
adjAllowance = allowanceReplenishmentEntry mk 7000 1000

-- (d) 経過勘定: 支払家賃のうち翌期分 ¥30,000 を前払費用へ繰り延べる。
adjPrepaid :: MinTransaction
adjPrepaid = prepaidExpenseEntry mk 30000 RentExpense

adjustments :: MinTransaction
adjustments = adjCOGS .+ adjDepreciation .+ adjAllowance .+ adjPrepaid

------------------------------------------------------------------
-- 4. 決算整理後残高 → 決算振替
------------------------------------------------------------------

postAdjustment :: MinTransaction
postAdjustment = preAdjustment .+ adjustments

-- 損益計算の対象は名目勘定 (費用 Cost / 収益 Revenue) のみ。
-- incomeSummaryAccount は対象の貸借差額を当期純利益/純損失として算定するので,
-- 実在勘定を含めた全台帳に直接適用すると差額が 0 になり意味を成さない。
nominalLedger :: MinTransaction
nominalLedger = EA.filter isNominal postAdjustment
  where
    isNominal x = let dv = (whatDiv . _hatBase) x
                  in dv == Cost || dv == Revenue

-- 決算振替: 損益勘定で当期純利益を算定し, 繰越利益剰余金へ振り替える。
-- (名目勘定の損益 + NetIncome) を経て NetIncome を RetainedEarnings に振り替える。
incomeSummary :: MinTransaction
incomeSummary = incomeSummaryAccount nominalLedger

-- 実在勘定 (期首+期中+整理後の資産/負債/資本) に当期純利益の振替を反映した残高。
afterClosing :: MinTransaction
afterClosing = realLedger .+ netIncomeTransfer (projByAccountTitle NetIncome incomeSummary
                                              .+ projByAccountTitle NetLoss  incomeSummary)
  where
    realLedger = EA.filter isReal postAdjustment
    isReal x = let dv = (whatDiv . _hatBase) x
               in dv == Assets || dv == Liability || dv == Equity

------------------------------------------------------------------
-- 検算
------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== ebex9: 決算の全体像・財務諸表 (第 24-25 章, 総合例) ==="
    putStrLn ""

    -- 期中取引・整理仕訳の貸借一致
    putStrLn "決算整理前残高 (期首 + 期中) 借/貸:"
    putStrLn $ "  Debit  = " ++ show (norm (decL preAdjustment))
    putStrLn $ "  Credit = " ++ show (norm (decR preAdjustment))
    putStrLn ""

    -- 当期純利益/純損失 (名目勘定の損益差額)
    let (side, profit) = diffRL nominalLedger
    let plLabel = case side of Credit -> "当期純利益"; Debit -> "当期純損失"; _ -> "損益なし"
    putStrLn $ "名目勘定の損益差額 (" ++ plLabel ++ "): " ++ show profit
    putStrLn ""

    -- 3. 8 桁精算表
    writeWorksheet "examples/basic/result/csv/ebex9_worksheet.csv" preAdjustment adjustments
    putStrLn "wrote: examples/basic/result/csv/ebex9_worksheet.csv (8 桁精算表)"

    -- 5. 繰越試算表 (決算振替後 — 実在勘定のみ残る)
    writePostClosingTrialBalance
        "examples/basic/result/csv/ebex9_post_closing_tb.csv" afterClosing
    putStrLn "wrote: examples/basic/result/csv/ebex9_post_closing_tb.csv (繰越試算表)"

    -- 6. B/S・P/L
    writeBS "examples/basic/result/csv/ebex9_bs.csv" afterClosing
    putStrLn "wrote: examples/basic/result/csv/ebex9_bs.csv (貸借対照表)"
    writePL "examples/basic/result/csv/ebex9_pl.csv" postAdjustment
    putStrLn "wrote: examples/basic/result/csv/ebex9_pl.csv (損益計算書)"
    putStrLn ""

    ------------------------------------------------------------------
    -- 検算 (不一致なら exitFailure)
    ------------------------------------------------------------------
    -- (1) 決算整理前残高が貸借一致
    let c1 = norm (decL preAdjustment) == norm (decR preAdjustment)
    -- (2) 整理仕訳が貸借一致
    let c2 = norm (decL adjustments) == norm (decR adjustments)
    -- (3) 決算振替後も貸借一致
    let c3 = norm (decL afterClosing) == norm (decR afterClosing)
    -- (4) 売上原価 = 期首 + 当期仕入 − 期末
    --     当期仕入 = 400,000, 期首 80,000, 期末 110,000 → 売上原価 370,000
    let cogs = norm (projByAccountTitle Purchases (bar postAdjustment))
        c4   = cogs == 370000
    -- (5) 繰越試算表は実在勘定 (資産/負債/資本) のみ。名目勘定 (Sales/Purchases や
    --     費用科目) は postClosingTrialBalanceRows が分類で除外する。
    let pcRows   = postClosingTrialBalanceRows afterClosing
        rowTitle r = case r of (_:t:_) -> t; _ -> T.empty
        titlesShown = Prelude.map rowTitle (drop 1 pcRows)   -- ヘッダ除く
        nominalShown = [ s | s <- titlesShown
                           , s `elem` Prelude.map (T.pack . show)
                                       [Sales, Purchases, WageExpenditure, Depreciation] ]
        c5 = null nominalShown
    -- (6) 繰越商品が期末残高 110,000 になっている
    let inv = norm (projByAccountTitle MerchandiseInventory (bar postAdjustment))
        c6  = inv == 110000

    putStrLn "--- 検算 ---"
    putStrLn $ "  (1) 整理前残高 貸借一致              : " ++ show c1
    putStrLn $ "  (2) 整理仕訳   貸借一致              : " ++ show c2
    putStrLn $ "  (3) 決算振替後 貸借一致              : " ++ show c3
    putStrLn $ "  (4) 売上原価 = 370,000              : " ++ show c4 ++ " (=" ++ show cogs ++ ")"
    putStrLn $ "  (5) 繰越試算表は実在勘定のみ (名目除外): " ++ show c5
    putStrLn $ "  (6) 繰越商品 = 期末 110,000          : " ++ show c6 ++ " (=" ++ show inv ++ ")"

    unless (and [c1, c2, c3, c4, c5, c6]) $ do
        putStrLn "ASSERTION FAILED in ebex9"
        exitFailure
    putStrLn ""
    putStrLn "OK: 簿記一巡 (期中 → 整理 → 精算表 → 振替 → 繰越試算表 → B/S・P/L) 検算成功"
