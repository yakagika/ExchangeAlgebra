{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getArgs)

import           Golden.WriteRows

main :: IO ()
main = do
    [outdir] <- getArgs
    createDirectoryIfMissing True outdir
    mapM_ (\(name, contents) -> TIO.writeFile (outdir ++ "/" ++ name) contents)
        writeRowsFixtures
    putStrLn "write-rows-0510 dump complete"
