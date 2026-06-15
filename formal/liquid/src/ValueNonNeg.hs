{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
module ValueNonNeg where

-- | A non-negative monetary magnitude (EA's MoneyDecimal / NonNegative.Double).
{-@ type NonNeg = {v:Double | 0.0 <= v} @-}

{-@ mkValue :: {x:Double | 0.0 <= x} -> NonNeg @-}
mkValue :: Double -> Double
mkValue x = x

{-@ addV :: NonNeg -> NonNeg -> NonNeg @-}
addV :: Double -> Double -> Double
addV x y = x + y

{-@ subClamp :: NonNeg -> NonNeg -> NonNeg @-}
subClamp :: Double -> Double -> Double
subClamp x y = if x >= y then x - y else 0.0

-- Invariant violation, shown for contrast. With the refined signature enabled
-- LiquidHaskell must REJECT it (raw subtraction does not preserve NonNeg).
-- {-@ subBad :: NonNeg -> NonNeg -> NonNeg @-}
subBad :: Double -> Double -> Double
subBad x y = x - y
