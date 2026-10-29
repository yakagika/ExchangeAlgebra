{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ExchangeAlgebra
import qualified ExchangeAlgebra.Accounting.Transfer as Transfer
import Control.Monad (unless)

data Unit = Yen
          | Dollor
          | Unit
          deriving (Show, Ord, Eq, Enum, Bounded, Generic)

instance Hashable Unit where

instance Element Unit where
    wildcard = Unit


instance BaseClass Unit where

type HatBase2 = HatBase (AccountTitles, Unit)

type RedAlg = Alg MoneyDecimal HatBase2

instance ExBaseClass HatBase2 where
    getAccountTitle (h :<(a,u)) = a
    setAccountTitle (h :<(a,u)) b = h :<(b,u)

main :: IO ()
main = do
    let x1 = 30000 .@ Not :<(Cash, Main.Yen) :: RedAlg
    print x1 -- > 30000.00:@Not:<(Cash,Yen)
    let x2 = 0 .@ Not :<(Cash, Main.Yen) :: RedAlg
    print x2  -- > 0
    let x3 = 3000 .@ Hat :<(Deposits,Main.Yen) .+ 3000 .@ Not :<(Cash, Main.Yen) :: RedAlg
    print x3 -- > 30000.00:@Not:<(Cash,Yen) .+ 3000.00:@Hat:<(Deposits,Yen)

    let x4 = 3000 .@ Hat :<(Cash, Main.Yen) .+ 3000 .@ Not :<(Cash, Main.Yen) :: RedAlg
    print $ (.^) (x4) -- > 3000.00:@Hat:<(Cash,Yen) + 3000.00:@Not:<(Deposits,Yen)
    print $ (.-) (x4) -- > 0
    print $ 2.0 .* x4 -- > 6000.00:@Hat:<(Cash,Yen) + 6000.00:@Not:<(Deposits,Yen)
    print $ norm (x4) -- > 6000.0

    let x5 =  3000 .@ Hat :<(Cash, Main.Yen) .+ 3000 .@ Not :<(Cash, Dollor) :: RedAlg
    print $ proj [Not:<(Cash,(.#))]  x5 -- > 3000.00:@Not:<(Cash,Dollor)
    print $ proj [HatNot:<((.#),Main.Yen)] x5 -- > 3000.00:@Not:<(Cash,Yen) .+ 3000.00:@Hat:<(Deposits,Yen)

    let x6 = 100.@Not :<(Cash, Main.Yen) :: RedAlg
    print $ (whatDiv  . _hatBase) x6   -- > Assets
    print $ (whatPIMO . _hatBase) x6   -- > PS
    print $ (whichSide . _hatBase) x6  -- > Debit
    print $ (whichSide . _hatBase) $ (.^) x6 -- > Credit

    print $ decR x3 -- > 3000.00:@Hat:<(Deposits,Yen)
    print $ decL x3 -- > 3000.00:@Not:<(Cash,Yen)
    print $ decP x3 -- > 3000.00:@Hat:<(Deposits,Yen)
    print $ decM x3 -- > 3000.00:@Not:<(Cash,Yen)

    let x7 = 100 .@ Not :< (Sales, Main.Yen)
          .+  50 .@ Not :< (ReceiptFee, Main.Yen)
          .+ 200 .@ Not :< (Purchases, Main.Yen)
          .+  50 .@ Not :< (RentExpense, Main.Yen)
          :: RedAlg
    incomeTable <- either (fail . show) pure $ Transfer.mkTransferTable
        [ (Sales, Main.Yen)       .-> (IncomeSummary, Main.Yen) |% Relabel
        , (ReceiptFee, Main.Yen)  .-> (IncomeSummary, Main.Yen) |% Relabel
        , (Purchases, Main.Yen)   .-> (IncomeSummary, Main.Yen) |% Relabel
        , (RentExpense, Main.Yen) .-> (IncomeSummary, Main.Yen) |% Relabel
        ]
    incomeEntries <- either (fail . show) pure $ Transfer.transferEntries incomeTable x7
    let afterIncome = x7 .+ incomeEntries
        summary = bar (projByAccountTitle IncomeSummary afterIncome)
    print afterIncome
    print summary
    unless (norm (bar (proj [HatNot :< (Sales, Main.Yen)
                           ,HatNot :< (ReceiptFee, Main.Yen)
                           ,HatNot :< (Purchases, Main.Yen)
                           ,HatNot :< (RentExpense, Main.Yen)] afterIncome)) == 0
            && norm (decL summary) == 100 && norm (decR summary) == 0) $
        fail "expected a debit loss of 100 in IncomeSummary"
    earningsTable <- either (fail . show) pure $ Transfer.mkTransferTable
        [(IncomeSummary, Main.Yen) .-> (RetainedEarnings, Main.Yen) |% Relabel]
    earningsEntries <- either (fail . show) pure $
        Transfer.transferEntries earningsTable afterIncome
    let afterEarnings = afterIncome .+ earningsEntries
        earnings = bar (projByAccountTitle RetainedEarnings afterEarnings)
    print afterEarnings
    unless (norm (bar (projByAccountTitle IncomeSummary afterEarnings)) == 0
            && norm (decL earnings) == 100 && norm (decR earnings) == 0) $
        fail "expected a debit loss of 100 in RetainedEarnings"
