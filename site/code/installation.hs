-- | Record the January 10 withdrawal with built-in accounts and currency units.
-- Algebra constructs terms; the account elements identify cash and deposits.
-- ANCHOR: program
module Main where

import ExchangeAlgebra

-- | Withdraw 3,000 yen from Bank A's savings account into cash.
withdrawal :: Alg MoneyDecimal (HatBase (AccountTitles, CountUnit))
withdrawal = 3000 .@ Not :< (Cash, Yen)
          .+ 3000 .@ Hat :< (Deposits, Yen)

-- | Print the term record of the withdrawal.
main :: IO ()
main = print withdrawal
-- ANCHOR_END: program
