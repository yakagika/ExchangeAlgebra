{-# LANGUAGE OverloadedStrings #-}

-- | Convert quantities to money without changing the quantity ledger.
-- Use mapPosting for values and units, and EJ.map to keep journal notes.
import ExchangeAlgebra
import qualified ExchangeAlgebra.Journal as EJ
import qualified Data.Map.Strict as M
import Control.Monad (unless)
import Data.Text (Text)

-- | Account, product identifier, and unit of a quantity or money posting.
type Base = HatBase (AccountTitles, Text, CountUnit)

-- | Transaction notes on a quantity ledger.
type Book = EJ.Journal Int Double Base

-- | Product prices in yen per unit; no price from another period is used.
type Prices = M.Map Text Double

-- | Convert product quantities to yen while retaining Hat/Not and other postings.
toYen :: Prices -> Alg Double Base -> Alg Double Base
toYen prices = mapPosting convert
  where
    convert :: Double -> Base -> (Double, Base)
    convert value posting = case base posting of
        (Products, productId, Amount) | Just price <- M.lookup productId prices
            -> (value * price, merge (hat posting) (Products, productId, Yen))
        _ -> (value, posting)

-- | Convert yen to quantities only for positive prices, avoiding division by zero.
toAmount :: Prices -> Alg Double Base -> Alg Double Base
toAmount prices = mapPosting convert
  where
    convert :: Double -> Base -> (Double, Base)
    convert value posting = case base posting of
        (Products, productId, Yen) | Just price <- M.lookup productId prices, price > 0
            -> (value / price, merge (hat posting) (Products, productId, Amount))
        _ -> (value, posting)

-- | Stop when a manually specified expected result differs.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual = unless (expected == actual) $
    fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Check conversion against fixed values, round trips, and preserved notes.
main :: IO ()
main = do
    let prices :: Prices
        prices = M.fromList [("g1", 3), ("g2", 0.5)]
        quantities = 1 .@ Hat :< (Cash, "-", Yen)
                  .+ 4 .@ Not :< (Products, "g1", Amount)
                  .+ 2 .@ Hat :< (Products, "g2", Amount) :: Alg Double Base
        journal = quantities EJ..| 1 :: Book
        expected = 1 .@ Hat :< (Cash, "-", Yen)
                .+ 12 .@ Not :< (Products, "g1", Yen)
                .+ 1 .@ Hat :< (Products, "g2", Yen) :: Alg Double Base
    check "quantity to yen" expected (toYen prices quantities)
    check "getter after conversion" 14
        $ norm
        $ toYen prices quantities
    check "round trip" quantities (toAmount prices expected)
    check "journal conversion" expected
        $ EJ.toAlg
        $ EJ.map (toYen prices) journal
    check "zero price is not divided" expected
        $ toAmount (M.fromList [("g1", 0)]) expected
    putStrLn "Conversion: all checks passed"
