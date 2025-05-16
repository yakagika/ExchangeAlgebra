


{-
レクチャー初級簿記の事例を交換代数で計算する
-}

-- Original
import qualified    ExchangeAlgebra         as EA
import              ExchangeAlgebra
import qualified    ExchangeAlgebra.Algebra.Transfer as ET
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

-- 時間をいれる
-- | 勘定科目と時間の基底
type ADBase = EA.HatBase (EA.AccountTitles,Day)

-- | 取引情報
type ADTransaction = EA.Alg NN.Double ADBase

d = fromGregorian

getDay :: ADBase -> Day
getDay (x:<(a,d)) = d


-- 1月5日　田中株式会社を設立し、株式100株を1株の払込金額￥80,000で発行。
-- 全株式の払込みを受け、株主から払込金額が普通預金口座に入金された。
-- 1/5 （借）普通預金　8,000,000　　（貸）資本金　8,000,000

t1_5 :: ADTransaction
t1_5     = 8000000 :@Not :<(Deposits, d 2024 4 1)
        .+ 8000000 :@Not :<(CapitalStock, d 2024 4 1)

-- 1月10日　普通預金口座から現金￥3,000,000を引き出した。
-- 1/10 （借）現　　金　3,000,000　　（貸）普通預金　3,000,000

t1_10 :: ADTransaction
t1_10    = 3000000 :@Not :<(Cash,d 2024 1 10)
        .+ 3000000 :@Hat :<(Deposits,d 2024 1 10)

-- 1月15日　現金￥1,200,000を当座預金口座に預け入れた。
-- 1/15 （借）当座預金　1,200,000　　（貸）現　　金　1,200,000

t1_15 :: ADTransaction
t1_15    = 1200000 :@Hat :<(Cash,d 2024 1 15)
        .+ 1200000 :@Not :<(CurrentDeposits,d 2024 1 15)


-- 1月20日　オフィス用品店から事務用品￥200,000を購入し、代金を現金で支払った。
-- 1/20 （借）備　　品　200,000　　（貸）現　　金　200,000

t1_20 :: ADTransaction
t1_20    = 200000 :@Hat :<(Cash,d 2024 1 20)
        .+ 200000 :@Not :<(Machinery,d 2024 1 20)

-- 1月25日　商品￥400,000を仕入れ、代金のうち￥200,000は現金で支払い、残額は掛けとした。
-- 1/25 （借）仕　　入　400,000　　（貸）現　　金　200,000
-- 　　　　　　　　　　　買掛金　200,000

t1_25 :: ADTransaction
t1_25    = 400000 :@Not :<(Purchases,d 2024 1 25)
        .+ 200000 :@Hat :<(Cash,d 2024 1 25)
        .+ 200000 :@Not :<(ReserveDepositPayable,d 2024 1 25)

-- 2月1日　取引先から現金￥300,000を借り入れた。
-- 2/1 （借）現　　金　300,000　　（貸）借入金　300,000

t2_1 :: ADTransaction
t2_1     = 300000 :@Not :<(Cash,d 2024 2 1)
        .+ 300000 :@Not :<(LoansPayable,d 2024 2 1)

-- 2月10日　得意先へ商品￥500,000を売り上げ、代金のうち￥100,000は現金で受け取り、残額は掛けとした。
-- 2/10 （借）現　　金　100,000　　（貸）売　　上　500,000
-- 　　　　売掛金　400,000

t2_10 :: ADTransaction
t2_10    = 100000 :@Not :<(Cash,d 2024 2 10)
        .+ 500000 :@Not :<(Sales,d 2024 2 10)
        .+ 400000 :@Not :<(AccountsReceivable,d 2024 2 10)

-- 2月15日　従業員に給料￥80,000を現金で支払った。
-- 2/15 （借）給　　料　80,000　　（貸）現　　金　80,000
t2_15 :: ADTransaction
t2_15    = 80000 :@Hat :<(Cash,d 2024 2 15)
        .+ 80000 :@Not :<(WageExpenditure,d 2024 2 15)

-- 2月20日　仕入先に対する買掛金￥150,000を現金で支払った。
-- 2/20 （借）買掛金　150,000　　（貸）現　　金　150,000

t2_20 :: ADTransaction
t2_20    = 150000 :@Hat :<(Cash,d 2024 2 20)
        .+ 150000 :@Hat :<(ReserveDepositPayable,d 2024 2 20)

t_total  = t1_5 .+ t1_10 .+ t1_15 .+ t1_20 .+ t1_25
        .+ t2_1 .+ t2_10 .+ t2_15 .+ t2_20

-- cashBalance2    = diffRL $ projByAccountTitle Cash q7

-- 8章 決算本手続き
------------------------------------------------------------------
-- 当期純利益の算定
sample :: MinTransaction
sample   = 1000000 :@Not:<Sales
        .+ 600000  :@Not:<Purchases
        .+ 180000  :@Not:<WageExpenditure

exp8A :: MinTransaction
exp8A = netIncomeTransfer $ incomeSummaryAccount sample

-- 当期純利益の振替
netIncomeTransfer' :: MinTransaction -> MinTransaction
netIncomeTransfer' ts
    =  transfer ts
    $  table
    $  Not:<NetIncome .-> Not:<RetainedEarnings |% id
    ++ Not:<NetLoss   .-> Not:<RetainedEarnings |% id

-- 主体をいれる

-- 物量簿記に変換する

-- 多数主体のシミュレーション

main :: IO ()
main = do
    let x = 100 :@Not:<(Cash, d 2024 1 2) :: ADTransaction
    print $ (whatDiv . _hatBase) x
    -- >>> Assets
    print $ (whatPIMO . _hatBase) x
    -- >>> PS
    print $ (whichSide . _hatBase) x
    -- >>> Credit
    let y = 100 :@Hat:<(Cash, d 2024 1 2) :: ADTransaction
    print $ (whichSide . _hatBase) y
    --- >>> Debit

    print $ projByAccountTitle Cash t1_10
    --- >>> 3000000 :@Not :<(Cash,d 2024 1 10)

    print $ decL t1_10
    --- >>> 3000000 :@Not :<(Cash,d 2024 1 10)
    print $ decR t1_10
    --- >>> 3000000.0:@Hat:<(Deposits,2024-01-10)
    print $ norm $ decR t1_10
    --- >>> 3000000.0
    let z  = 100 :@Not:<(Cash, d 2024 1 2)
          .+  50 :@Hat:<(Cash, d 2024 1 2) :: ADTransaction
    print $ bar z
    -- >>> 50.0:@Not:<(Cash,2024-01-02)

    let t_total  = t1_5 .+ t1_10 .+ t1_15 .+ t1_20 .+ t1_25
                .+ t2_1 .+ t2_10 .+ t2_15 .+ t2_20
    writeJournal "exsample/result/csv/t_total_journal.csv" t_total getDay
    writeCompoundTrialBalance "exsample/result/csv/t_total_CTB.csv" t_total

    print $ (norm (decR sample)) - (norm (decL sample))
    -- >>> 220000.0
    print $ diffRL sample
    -- >>>(Debit,220000.0)

    print $ sample .+ 220000 :@Not:<NetIncome
    -- >>> 1000000.0:@Not:<Sales .+ 600000.0:@Not:<Purchases
    --  .+ 180000.0:@Not:<WageExpenditure
    --  .+ 220000.0:@Not:<NetIncome
    print $ incomeSummaryAccount sample
    -- >>> 1000000.0:@Not:<Sales .+ 600000.0:@Not:<Purchases
    --  .+ 180000.0:@Not:<WageExpenditure
    --  .+ 220000.0:@Not:<NetIncome

    let sample2 = incomeSummaryAccount sample
    writePL "exsample/result/csv/sample_PL.csv" sample2

    let tf = createTransfer $ Not:<NetIncome .-> Not:<RetainedEarnings |% id
    print $ tf $ incomeSummaryAccount sample
    -- >>> 1000000.0:@Not:<Sales
    --  .+ 600000.0:@Not:<Purchases
    --  .+ 180000.0:@Not:<WageExpenditure
    --  .+ 220000.0:@Not:<RetainedEarnings
    print $ netIncomeTransfer $ incomeSummaryAccount sample
    -- >>> 1000000.0:@Not:<Sales
    --  .+ 600000.0:@Not:<Purchases
    --  .+ 180000.0:@Not:<WageExpenditure
    --  .+ 220000.0:@Not:<RetainedEarnings

