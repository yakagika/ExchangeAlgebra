module Main where
import ExchangeAlgebra.Algebra
import ExchangeAlgebra.Algebra.Base
import ExchangeAlgebra.Algebra.Transfer (finalStockTransfer)
type A = Alg Double (HatBase AccountTitles)
main :: IO ()
main = mapM_ pr concreteAccountTitles
  where
    pr t = putStrLn (show t ++ "\t" ++ r t ++ "\t" ++ show (classifyAccountDivision t))
    r RetainedEarnings = "SELF"
    r t = case show (finalStockTransfer (1 .@ Not:<t :: A)) of
        s | s == show (1 .@ Not:<t :: A)                     -> "Nothing"
          | s == show (1 .@ Not:<RetainedEarnings :: A)      -> "Keep"
          | s == show (1 .@ Hat:<RetainedEarnings :: A)      -> "Flip"
          | otherwise                                        -> "UNEXPECTED:" ++ s
