{-
  ebex7 — 貸倒引当金・固定資産・減価償却
  レクチャー初級簿記 第 16-18 章対応 (数値は変換済; 桁感のみ踏襲)

  扱う論点:
    第 16 章 貸倒れ (引当金充当/不足分は貸倒損失, 償却債権取立益),
             差額補充法 (allowanceReplenishmentEntry) と
             洗替法 (allowanceResetEntries)
    第 17 章 固定資産の取得 (付随費用込み)・売却 (売却益/売却損, 間接法の累計額考慮)
    第 18 章 減価償却 (定額法), 間接法 (depreciationIndirectEntry) と
             直接法 (depreciationDirectEntry), 月次按分の例

  決算整理の builder は ExchangeAlgebra.Bookkeeping から。基底注入 mk は (:<)。
  集約は bar/norm を明示。値は非負, 構築は '.@'。
-}

import qualified ExchangeAlgebra            as EA
import           ExchangeAlgebra
import           ExchangeAlgebra.Bookkeeping
import           System.Exit                (exitFailure)
import           Control.Monad              (unless)

type MinBase        = EA.HatBase EA.AccountTitles
type MinTransaction = EA.Alg MoneyDecimal MinBase

-- | 基底注入: 勘定科目だけの単純基底なので (:<) をそのまま渡す。
mk :: MkBase MinBase
mk = (:<)

------------------------------------------------------------------
-- 第 16 章 貸倒れと貸倒引当金
------------------------------------------------------------------

-- (1) 前期発生の売掛金 ¥40,000 が貸し倒れた。貸倒引当金の残高は ¥50,000 ある。
--     引当金を充当する (全額が引当金でまかなえるケース)。
-- (借) 貸倒引当金 40,000 (貸) 売掛金 40,000
ex16_writeOffWithinAllowance :: MinTransaction
ex16_writeOffWithinAllowance
    =  40000 .@ Hat :< AllowanceForDoubtfulAccounts
    .+ 40000 .@ Hat :< AccountsReceivable

-- (2) 前期発生の売掛金 ¥70,000 が貸し倒れた。貸倒引当金の残高は ¥50,000 のみ。
--     不足 ¥20,000 は貸倒損失とする。
-- (借) 貸倒引当金 50,000 / 貸倒損失 20,000 (貸) 売掛金 70,000
ex16_writeOffShortfall :: MinTransaction
ex16_writeOffShortfall
    =  50000 .@ Hat :< AllowanceForDoubtfulAccounts
    .+ 20000 .@ Not :< BadDebtLoss
    .+ 70000 .@ Hat :< AccountsReceivable

-- (3) 当期に発生し当期に貸し倒れた売掛金 ¥10,000 は全額貸倒損失。
-- (借) 貸倒損失 10,000 (貸) 売掛金 10,000
ex16_writeOffCurrentPeriod :: MinTransaction
ex16_writeOffCurrentPeriod
    =  10000 .@ Not :< BadDebtLoss
    .+ 10000 .@ Hat :< AccountsReceivable

-- (4) 前期に貸倒処理した債権 ¥8,000 を当期に現金回収した (償却債権取立益)。
-- (借) 現金 8,000 (貸) 償却債権取立益 8,000
ex16_recovery :: MinTransaction
ex16_recovery
    =  8000 .@ Not :< Cash
    .+ 8000 .@ Not :< RecoveryOfBadDebts

-- (5) 差額補充法による決算時の引当: 見積額 ¥30,000, 残高 ¥18,000。
--     差額 ¥12,000 を繰り入れる。
ex16_allowanceReplenish :: MinTransaction
ex16_allowanceReplenish = allowanceReplenishmentEntry mk 30000 18000

-- (6) 洗替法による決算時の引当: 残高 ¥18,000 を全額戻入し,
--     見積額 ¥30,000 を全額繰り入れる。
ex16_allowanceReset :: MinTransaction
ex16_allowanceReset = allowanceResetEntries mk 30000 18000

------------------------------------------------------------------
-- 第 17 章 固定資産
------------------------------------------------------------------

-- (1) 取得 (付随費用込み): 備品 ¥500,000 を購入し, 引取運賃 ¥20,000 を含めた
--     ¥520,000 を当座預金から支払った。付随費用は取得原価に算入する。
-- (借) 備品 520,000 (貸) 当座預金 520,000
ex17_acquire :: MinTransaction
ex17_acquire
    =  520000 .@ Not :< Fixtures
    .+ 520000 .@ Hat :< CurrentDeposits

-- (2) 売却益 (間接法): 取得原価 ¥600,000, 減価償却累計額 ¥360,000 (簿価 ¥240,000)
--     の備品を ¥300,000 で売却し代金は現金で受け取った。売却益 ¥60,000。
-- (借) 減価償却累計額 360,000 / 現金 300,000
--   (貸) 備品 600,000 / 固定資産売却益 60,000
ex17_sellWithGain :: MinTransaction
ex17_sellWithGain
    =  360000 .@ Hat :< AccumulatedDepreciation  -- 累計額を取り崩す
    .+ 300000 .@ Not :< Cash
    .+ 600000 .@ Hat :< Fixtures
    .+ 60000  .@ Not :< GainOnSalesOfFixedAssets

-- (3) 売却損 (間接法): 取得原価 ¥600,000, 減価償却累計額 ¥360,000 (簿価 ¥240,000)
--     の備品を ¥200,000 で売却し代金は現金で受け取った。売却損 ¥40,000。
-- (借) 減価償却累計額 360,000 / 現金 200,000 / 固定資産売却損 40,000
--   (貸) 備品 600,000
ex17_sellWithLoss :: MinTransaction
ex17_sellWithLoss
    =  360000 .@ Hat :< AccumulatedDepreciation
    .+ 200000 .@ Not :< Cash
    .+ 40000  .@ Not :< LossOnSalesOfFixedAssets
    .+ 600000 .@ Hat :< Fixtures

------------------------------------------------------------------
-- 第 18 章 減価償却 (定額法)
------------------------------------------------------------------

-- 建物 取得原価 ¥1,200,000, 耐用年数 20 年, 残存価額 0, 定額法。
-- 1 年分の減価償却費 = 1,200,000 / 20 = ¥60,000。

annualDepreciation :: MoneyDecimal
annualDepreciation = 60000

-- (1) 間接法: (借) 減価償却費 60,000 (貸) 減価償却累計額 60,000
ex18_indirect :: MinTransaction
ex18_indirect = depreciationIndirectEntry mk annualDepreciation

-- (2) 直接法: (借) 減価償却費 60,000 (貸) 建物 60,000
ex18_direct :: MinTransaction
ex18_direct = depreciationDirectEntry mk annualDepreciation Building

-- (3) 月次按分: 期中の 10 月 1 日取得 (備品 ¥360,000, 耐用年数 5 年, 定額法)。
--     当期は 10-3 月の 6 か月分を月割計上する。
--     年額 = 360,000 / 5 = 72,000。6 か月分 = 72,000 * 6/12 = ¥36,000。
ex18_monthly :: MinTransaction
ex18_monthly = depreciationIndirectEntry mk (72000 * 6 `divDec` 12)
  where
    -- MoneyDecimal の安全な按分 (整数月割なので割り切れる)
    divDec a b = a / fromIntegral b

------------------------------------------------------------------
-- 検算
------------------------------------------------------------------

-- 検算対象: builder/手書き仕訳すべてが貸借一致すること。
allBalanced :: [(String, MinTransaction)]
allBalanced =
    [ ("ex16_writeOffWithinAllowance", ex16_writeOffWithinAllowance)
    , ("ex16_writeOffShortfall",       ex16_writeOffShortfall)
    , ("ex16_writeOffCurrentPeriod",   ex16_writeOffCurrentPeriod)
    , ("ex16_recovery",                ex16_recovery)
    , ("ex16_allowanceReplenish",      ex16_allowanceReplenish)
    , ("ex16_allowanceReset",          ex16_allowanceReset)
    , ("ex17_acquire",                 ex17_acquire)
    , ("ex17_sellWithGain",            ex17_sellWithGain)
    , ("ex17_sellWithLoss",            ex17_sellWithLoss)
    , ("ex18_indirect",                ex18_indirect)
    , ("ex18_direct",                  ex18_direct)
    , ("ex18_monthly",                 ex18_monthly)
    ]

main :: IO ()
main = do
    putStrLn "=== ebex7: 貸倒引当金・固定資産・減価償却 (第 16-18 章) ==="
    putStrLn ""

    -- 差額補充法と洗替法の引当額を見せる
    putStrLn "貸倒引当金 (見積 30,000, 残高 18,000):"
    putStrLn $ "  差額補充法 繰入額 = "
             ++ show (norm (projByAccountTitle ProvisionForDoubtfulAccounts ex16_allowanceReplenish))
    putStrLn $ "  洗替法     繰入額 = "
             ++ show (norm (projByAccountTitle ProvisionForDoubtfulAccounts ex16_allowanceReset))
             ++ " / 戻入額 = "
             ++ show (norm (projByAccountTitle ReversalOfAllowanceForDoubtfulAccounts ex16_allowanceReset))
    putStrLn ""
    putStrLn "減価償却費 (建物 年額) = "
    putStrLn $ "  間接法 = " ++ show (norm (projByAccountTitle Depreciation ex18_indirect))
    putStrLn $ "  直接法 = " ++ show (norm (projByAccountTitle Depreciation ex18_direct))
    putStrLn $ "  月次按分 (6 か月) = " ++ show (norm (projByAccountTitle Depreciation ex18_monthly))
    putStrLn ""

    -- 全仕訳まとめの試算表
    let allEntries = foldr (.+) Zero (Prelude.map snd allBalanced)
    writeCompoundTrialBalance "examples/basic/result/csv/ebex7_trial_balance.csv" allEntries
    putStrLn "wrote: examples/basic/result/csv/ebex7_trial_balance.csv"
    putStrLn ""

    -- 検算: 各仕訳が貸借一致
    let bad = [ name | (name, e) <- allBalanced, norm (decL e) /= norm (decR e) ]
    unless (null bad) $ do
        putStrLn $ "ASSERTION FAILED (unbalanced): " ++ show bad
        exitFailure
    putStrLn "OK: every entry has debit total == credit total"
