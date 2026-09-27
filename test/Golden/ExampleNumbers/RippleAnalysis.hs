-- | Reproduce the coefficient matrix and analysis calls from the ripple example.
module Golden.ExampleNumbers.RippleAnalysis (numericRows) where

import           Data.Array.IO (IOArray, getAssocs, newListArray)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import           Data.Word (Word64)
import           GHC.Float (castDoubleToWord64)
import           Numeric (showHex)
import           System.Random (RandomGen (genWord32), StdGen, mkStdGen, randomR)

import           ExchangeAlgebra.Simulate.Analysis (leontiefInverse, rippleEffect)

-- | The example's nine producing industries, excluding final demand.
type Industry = Int

-- | One coefficient, Leontief inverse entry, or ripple entry.
type NumericRow = (String, Industry, Industry, Word64, String)

-- | Reproduce the example's first term with seed 42 and in-house ratio 0.4.
coefficientColumns :: M.Map Industry [Double]
coefficientColumns = fst $ foldl' addColumn (M.empty, mkStdGen 42) [1 .. 10]
  where
    addColumn (columns, generator) industry =
        let (samples, nextGenerator) = drawSamples generator
            total = sum samples
            coefficients = map (\sample -> sample / total * 0.4) samples
        in (M.insert industry coefficients columns, nextGenerator)

    drawSamples :: StdGen -> ([Double], StdGen)
    drawSamples generator =
        let (values, nextGenerator) = draw 9 (skipGenerator generator)
            clipped = map (\value -> if value < 0.1 then 0 else value) values
        in (clipped ++ [0], nextGenerator)

    draw 0 generator = ([], generator)
    draw remaining generator =
        let (value, nextGenerator) = randomR (0, 1.0) generator
            (values, finalGenerator) = draw (remaining - 1) nextGenerator
        in (value : values, finalGenerator)

    skipGenerator generator =
        foldl' (\current _ -> snd (genWord32 current)) generator [1 .. 1000]

-- | Calculate the first-term input matrix, inverse, and ninth-industry ripple.
numericRows :: IO [NumericRow]
numericRows = do
    let entries =
            [ coefficient row column
            | row <- [1 .. 9]
            , column <- [1 .. 9]
            ]
    matrix <- newListArray ((1, 1), (9, 9)) entries
        :: IO (IOArray (Industry, Industry) Double)
    inverse <- leontiefInverse matrix
    ripple <- rippleEffect 9 inverse
    coefficientRows <- pure $ zipWith (makeRow "inputCoefficient") indexes entries
    inverseRows <- map (uncurry (makeRow "leontiefInverse")) <$> getAssocs inverse
    rippleRows <- map (uncurry (makeRow "rippleEffect")) <$> getAssocs ripple
    pure (coefficientRows ++ inverseRows ++ rippleRows)
  where
    indexes = [(row, column) | row <- [1 .. 9], column <- [1 .. 9]]

    coefficient row column =
        case M.lookup column coefficientColumns of
            Just values -> case drop (row - 1) values of
                value : _ -> value
                [] -> 0
            Nothing -> 0

    makeRow name (row, column) value =
        (name, row, column, castDoubleToWord64 value, show value)
