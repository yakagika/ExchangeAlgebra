-- | Serialize text tables as CSV.
--
-- Import this module for 'writeCSV' and 'csvTranspose'.
module ExchangeAlgebra.IO.Output.Csv
    ( writeCSV
    , csvTranspose
    ) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (openFile, IOMode(WriteMode), hClose)

-- | Transpose a matrix of Text, padding shorter rows with empty Text.
csvTranspose :: [[T.Text]] -> [[T.Text]]
csvTranspose [] = []
csvTranspose mx = [ [ getCell r i | r <- mx ] | i <- [0 .. maxLen - 1] ]
  where
    maxLen = L.maximum (L.map L.length mx)
    getCell row i
        | i < L.length row = row !! i
        | otherwise        = T.empty

-- | Write a matrix of Text as a CSV file. Each cell is quoted.
writeCSV :: FilePath -> [[T.Text]] -> IO ()
writeCSV path rows = do
    h <- openFile path WriteMode
    mapM_ (TIO.hPutStrLn h . toCsvLine) rows
    hClose h
  where
    toCsvLine = T.intercalate (T.pack ",") . L.map quoteCell
    quoteCell t = T.concat [T.pack "\"", T.replace (T.pack "\"") (T.pack "\"\"") t, T.pack "\""]
