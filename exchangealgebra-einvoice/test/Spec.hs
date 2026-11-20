module Main (main) where

import qualified DocumentSpec
import qualified JournalSpec

-- | Run the document and journal tests.
main :: IO ()
main = do
    DocumentSpec.runTests
    JournalSpec.runTests
