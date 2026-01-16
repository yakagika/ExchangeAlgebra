module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest  [ "-isrc"
                , "src/ExchangeAlgebra.hs"
                , "src/ExchangeAlgebraJournal.hs"]