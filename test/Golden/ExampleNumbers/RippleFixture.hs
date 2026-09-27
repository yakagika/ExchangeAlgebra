{-# LANGUAGE OverloadedStrings #-}

-- | Render the ripple analysis output as a stable numeric fixture.
module Golden.ExampleNumbers.RippleFixture
    ( fixtureDir
    , fixtureName
    , renderFixture
    ) where

import qualified Data.Text as T
import           Numeric (showHex)

import           Golden.ExampleNumbers.RippleAnalysis (numericRows)

-- | Directory holding the P1 example-number fixtures.
fixtureDir :: FilePath
fixtureDir = "test/fixtures/example-numbers-p1"

-- | File containing the ripple matrix readouts.
fixtureName :: FilePath
fixtureName = "ripple-analysis.tsv"

-- | Render schema, baseline revision, and exact Double bits.
renderFixture :: IO T.Text
renderFixture = do
    rows <- numericRows
    pure $ T.unlines (header : map renderRow rows)
  where
    header =
        "# example-numbers-p1 ripple-analysis; schema 1; commit "
            <> "2a83fe178d47a1024e0e566794f39533a12b4c20"

    renderRow (name, row, column, bits, value) =
        T.intercalate "\t"
            [ T.pack name
            , T.pack (show row)
            , T.pack (show column)
            , T.pack ("0x" ++ replicate (16 - length digits) '0' ++ digits)
            , T.pack value
            ]
      where
        digits = showHex bits ""
