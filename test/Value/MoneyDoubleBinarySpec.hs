-- | Binary compatibility tests for the Algebra value layer.
-- The binary package's Double instance is the byte and decoder oracle.
-- Start with 'runTests' for exhaustive boundary cases and generated inputs.
module Value.MoneyDoubleBinarySpec (runTests) where

import Control.Monad (unless)
import qualified Data.Binary as Binary
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import System.Exit (exitFailure)
import Test.QuickCheck
import ExchangeAlgebra.Algebra.Value (MoneyDouble(..), toDouble)

-- | Check bytes and decoded bits, retaining binary's behavior for NaN and -0.
-- Finite inputs must additionally recover their original numeric value.
compatible :: Double -> Property
compatible value = conjoin
    [ Binary.encode (MoneyDouble value) === Binary.encode value
    , castDoubleToWord64 wrapped === castDoubleToWord64 bare
    , counterexample "finite value roundtrip" finiteRoundtrip
    ]
  where
    wrapped = toDouble (Binary.decode (Binary.encode (MoneyDouble value)))
    bare = Binary.decode (Binary.encode value) :: Double
    finiteRoundtrip :: Bool
    finiteRoundtrip
        | isNaN value || isInfinite value = True
        | otherwise = wrapped == value

-- | Include every binary exponent and signed special and mantissa boundaries.
boundaryValues :: [Double]
boundaryValues = signed
    ( [ 0, 5e-324, 1e-320, 1 / 0, 0 / 0
      , 2 ** 31 - 1, 2 ** 31, 2 ** 31 + 1
      , encodeFloat (2 ^ (53 :: Int) - 1) 971
      ]
      ++ [2 ^^ exponent | exponent <- [-1074 .. 1023 :: Int]]
    )
    ++ map castWord64ToDouble [0x7ff0000000000001, 0xfff8000000000001]
  where
    signed :: [Double] -> [Double]
    signed values = values ++ map negate values

-- | Fail the suite if a generated or deterministic compatibility check fails.
checkProperty :: String -> Property -> IO ()
checkProperty label law = do
    result <- quickCheckWithResult stdArgs { maxSuccess = 1000, chatty = False } law
    unless (isSuccess result) $ do
        putStrLn ("[FAIL] MoneyDouble Binary " ++ label)
        putStr (output result)
        exitFailure
    putStrLn ("[PASS] MoneyDouble Binary " ++ label)

-- | Check deterministic boundaries, arbitrary Double values, and arbitrary bits.
runTests :: IO ()
runTests = do
    checkProperty "special values and every exponent" $ once $
        conjoin [counterexample (show value) (compatible value) | value <- boundaryValues]
    checkProperty "arbitrary Double" (property compatible)
    checkProperty "arbitrary IEEE-754 bits" $
        forAll arbitrary (compatible . castWord64ToDouble)
