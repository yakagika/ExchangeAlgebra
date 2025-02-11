


{-
レクチャー初級簿記の事例を交換代数で計算する
-}

-- Original
import qualified    ExchangeAlgebra         as EA
import              ExchangeAlgebra
import qualified    ExchangeAlgebra.Transfer as ET
import              ExchangeAlgebra.Simulate

-- Other
import qualified    Number.NonNegative      as NN
import qualified    Numeric                 as N
import              Number.NonNegative

-- Day
import qualified    Data.Time           as Time
import              Data.Time

-- * 勘定科目だけの通常の簿記
-- | 勘定科目だけの交換代数基底
type MinBase = EA.HatBase EA.AccountTitles

-- | 取引情報
type MinTransaction = EA.Alg NN.Double MinBase


-- ** 3章例題
------------------------------------------------------------------

exp3 :: MinTransaction
exp3    =  350000 .@Not :<Cash                   -- 現金
        .+ 410000 .@Not :<Deposits               -- 普通預金
        .+ 270000 .@Not :<CurrentDeposits        -- 当座預金
        .+ 390000 .@Not :<AccountsReceivable     -- 売掛金
        .+ 420000 .@Not :<Products               -- 商品
        .+ 340000 .@Not :<Machinery              -- 備品
        .+ 620000 .@Not :<Vehicle                -- 車両運搬具
        .+ 180000 .@Not :<ReserveDepositPayable  -- 買掛金
        .+ 320000 .@Not :<LoansPayable           -- 借入金
        .+ 2000000.@Not :<CapitalStock           -- 資本金
        .+ 300000 .@Not :<RetainedEarnings       -- 繰越利益剰余金

-- 3章 演習問題
q3 :: MinTransaction
q3       = 2000000.@Not :<CapitalStock           -- 資本金
        .+ 280000 .@Not :<ReserveDepositPayable  -- 買掛金
        .+ 250000 .@Not :<Cash                   -- 現金
        .+ 400000 .@Not :<LoansPayable           -- 借入金
        .+ 450000 .@Not :<Deposits               -- 普通預金
        .+ 400000 .@Not :<AccountsReceivable     -- 売掛金
        .+ 530000 .@Not :<Products               -- 商品
        .+ 500000 .@Not :<Machinery              -- 備品
        .+ 350000 .@Not :<CurrentDeposits        -- 当座預金
        .+ 600000 .@Not :<Vehicle                -- 車両運搬具

-- 繰越利益剰余金を含んだ回答
q3A :: MinTransaction
q3A = q3 .+ y .@Not :<RetainedEarnings
    where
        (x,y) = diffRL q3


-- 4章 例題
------------------------------------------------------------------
exp4 :: MinTransaction
exp4    = 600000 .@Not:<CostOfGoodsSold  -- 売上原価
       .+ 520000 .@Not:<WageExpenditure  -- 給料
       .+ 80000  .@Not:<BusinessTrip     -- 旅費交通費
       .+ 70000  .@Not:<Commutation      -- 通信費
       .+ 30000  .@Not:<SuppliesExpenses -- 消耗品費
       .+ 50000  .@Not:<UtilitiesExpense -- 水道光熱費
       .+ 300000 .@Not:<RentExpense      -- 支払家賃
       .+ 300000 .@Not:<NetIncome        -- 当期純利益
       .+ 1700000.@Not:<Sales            -- 売上高
       .+ 150000 .@Not:<ReceiptFee       -- 受取手数料
       .+ 100000 .@Not:<InterestEarned   -- 受取利息

-- 4章 演習問題
q4 :: MinTransaction
q4       = 150000 .@Not:<ReceiptFee       -- 受取利息
        .+ 780000 .@Not:<WageExpenditure  -- 給料
        .+ 150000 .@Not:<BusinessTrip     -- 旅費交通費
        .+ 420000 .@Not:<RentExpense      -- 支払家賃
        .+ 2500000.@Not:<Sales            -- 売上高
        .+ 90000  .@Not:<UtilitiesExpense -- 水道光熱費
        .+ 140000 .@Not:<Commutation      -- 通信費
        .+ 840000 .@Not:<Purchases        -- 仕入
        .+ 220000 .@Not:<ReceiptFee       -- 受取手数料

incomeOrLoss :: MinTransaction -> MinTransaction
incomeOrLoss xs | x == Credit = y :@Not:<NetLoss
                | x == Debit  = y :@Not:<NetIncome
    where
        (x,y) = diffRL xs

q4A :: MinTransaction
q4A = q4 .+ (incomeOrLoss q4)

q4A' = ET.incomeSummaryAccount q4

-- 5章
------------------------------------------------------------------

-- 時間をいれる
-- | 勘定科目と時間の基底
type ADBase = EA.HatBase (EA.AccountTitles,Day)

-- | 取引情報
type ADTransaction = EA.Alg NN.Double ADBase

d = fromGregorian

getDay :: ADBase -> Day
getDay (x:<(a,d)) = d


-- 減少はHat,増加はNotで記載する.

-- ４月１日 瑞穂株式会社の設立にあたり,株式40株を1株の払込金額￥50,000で発行し,
-- 全株式の払込みを受け,株主から払込金額が取引銀行の普通預金口座に入金された。
-- ４/１（借）普通預金　2,000,000　　（貸）資　本　金　2,000,000

exp5_1 :: ADTransaction
exp5_1   = (40 * 50000):@Not:<(Deposits,d 2024 4 1)
        .+ (40 * 50000):@Not:<(CapitalStock,d 2024 4 1)

-- ２日 普通預金口座から現金￥1,000,000を引き出した。
-- ４/２（借）現　　金  1,000,000　　（貸）普通預金    1,000,000

exp5_2 :: ADTransaction
exp5_2   = 1000000:@Not:<(Cash,d 2024 4 2)
        .+ 1000000:@Hat:<(Deposits,d 2024 4 2)


-- ３日 現金￥500,000を当座預金口座に預け入れた。
-- ４/３（借）当座預金  　500,000　　（貸）現　　金    　500,000

exp5_3 :: ADTransaction
exp5_3   = 5000000:@Hat:<(Cash,d 2024 4 3)
        .+ 5000000:@Not:<(Deposits,d 2024 4 3)

-- ４日 事務用品販売会社から事務用デスク￥100,000を購入し，代金は現金で支払った。
-- ４/４（借）備　　品　  100,000　　（貸）現　　金　    100,000
exp5_4 :: ADTransaction
exp5_4   = 100000 .@Not :<(Machinery,d 2024 4 4)
        .+ 100000 .@Hat:<(Cash, d 2024 4 4)

-- ７日 仕入先から商品￥200,000を仕入れ，代金のうち￥100,000は現金で支払い残額は掛けとした。
-- ４/７（借）仕　　入　  200,000　　（貸）現　　金　    100,000
--                                         買 掛 金　    100,000
exp5_5 :: ADTransaction
exp5_5   = 200000 .@Not :<(Purchases,d 2024 4 7)
        .+ 100000 .@Hat:<(Cash, d 2024 4 7)
        .+ 100000 .@Not:<(ReserveDepositPayable, d 2024 4 7)

-- 10日 取引銀行より現金￥150,000を借り入れた。
-- ４/10（借）現　　金　  150,000　　（貸）借 入 金　    150,000
exp5_6 :: ADTransaction
exp5_6   = 150000 .@Not :<(Cash,d 2024 4 10)
        .+ 150000 .@Not:<(LoansPayable, d 2024 4 10)

-- 20日 得意先へ商品￥360,000を売り上げ，代金のうち￥60,000は現金で受け取り残額は掛けとした。
-- ４/20（借）現　　金　   60,000　　（貸）売　　上　    360,000
--            売 掛 金　  300,000
exp5_7 :: ADTransaction
exp5_7   = 60000  .@Not :<(Cash,d 2024 4 20)
        .+ 360000 .@Not:<(Sales, d 2024 4 20)
        .+ 300000 .@Not:<(AccountsReceivable, d 2024 4 20)

-- 25日 従業員に給料￥50,000を現金で支払った。
-- ４/25（借）給　　料　　 50,000　  （貸）現　　金　     50,000
exp5_8 :: ADTransaction
exp5_8   = 50000 .@Not :<(WageExpenditure,d 2024 4 25)
        .+ 50000 .@Hat:<(Cash, d 2024 4 25)

-- 30日 仕入先に対する買掛金￥50,000を現金で支払った。
-- ４/30（借）買 掛 金　   50,000　　（貸）現　　金　     50,000
exp5_9 :: ADTransaction
exp5_9   = 50000 .@Hat :<(ReserveDepositPayable,d 2024 4 30)
        .+ 50000 .@Hat:<(Cash, d 2024 4 30)

exp5_A   = exp5_1
        .+ exp5_2
        .+ exp5_3
        .+ exp5_4
        .+ exp5_5
        .+ exp5_6
        .+ exp5_7
        .+ exp5_8
        .+ exp5_9

-- 7章 合計残高試算表
------------------------------------------------------------------
-- 6章総勘定元帳は飛ばす

q7 :: MinTransaction
q7   = 3000000.@Not:<Cash                  .+ 3000000.@Not:<CapitalStock     -- 現金 - 資本金
    .+ 250000 .@Not:<Cash                  .+ 250000 .@Hat:<Deposits         -- 現金 - 普通預金
    .+ 1200000.@Hat:<Cash                  .+ 1200000.@Not:<Machinery        -- 現金 - 備品
    .+ 800000 .@Hat:<Cash                  .+ 800000 .@Not:<Purchases        -- 現金 - 仕入れ
    .+ 5000   .@Hat:<Cash                  .+ 5000   .@Not:<InterestExpense  -- 現金 - 支払利息
    .+ 700000 .@Not:<ReserveDepositPayable .+ 700000 .@Not:<Purchases        -- 買掛金 - 仕入れ
    .+ 500000 .@Not:<Sales                 .+ 500000 .@Not:<Deposits         -- 売上 - 普通預金
    .+ 300000 .@Not:<WageExpenditure       .+ 300000 .@Hat:<Deposits         -- 給料 - 普通預金
    .+ 1000000.@Not:<Deposits              .+ 1000000.@Not:<LoansPayable     -- 普通預金 - 借入金

-- バー演算で計算できることを示す

-- 現金のみの合計残高を計算してみる
cashDebitTotal  = norm $ decR $ projByAccountTitle Cash q7
cashCreditTotal = norm $ decL $ projByAccountTitle Cash q7
cashBalance1    = bar $ projByAccountTitle Cash q7
cashBalance2    = diffRL $ projByAccountTitle Cash q7

-- 8章 決算本手続き
------------------------------------------------------------------
-- 当期純利益の算定
exp8 :: MinTransaction
exp8     = 2000000 .@Not:<Sales
        .+ 800000  .@Not:<Purchases
        .+ 140000  .@Not:<WageExpenditure

exp8A :: MinTransaction
exp8A = netIncomeTransfer $ incomeSummaryAccount exp8

-- 当期純利益の振替
netIncomeTransfer' :: MinTransaction -> MinTransaction
netIncomeTransfer'
    =  createTransfer
    $  Not:<NetIncome .-> Not:<RetainedEarnings |% id
    ++ Not:<NetLoss   .-> Not:<RetainedEarnings |% id

-- 主体をいれる

-- 物量簿記に変換する

-- 多数主体のシミュレーション

main :: IO ()
main = do
    print exp3
    writeBS "exsample/result/csv/exp3.csv" exp3
    print "---"
    print q3A
    writeBS "exsample/result/csv/q3.csv" q3A
    print "---"
    print exp4
    writePL "exsample/result/csv/exp4.csv" exp4
    print "---"
    print q4A
    print q4A'
    writeBS "exsample/result/csv/q4.csv" q4A
    print "---"
    print exp5_A
    writeJournal "exsample/result/csv/exp5.csv" exp5_A getDay
    print "---"
    print cashDebitTotal
    print cashCreditTotal
    print cashBalance1
    print cashBalance2
    writeCompoundTrialBalance "exsample/result/csv/q7.csv" q7
    print "---"
    print exp8
    print $ incomeSummaryAccount exp8
    print $ netIncomeTransfer $ incomeSummaryAccount exp8
    print $ netIncomeTransfer' $ incomeSummaryAccount exp8
    writePL "exsample/result/csv/exp8A.csv" $ incomeSummaryAccount exp8

