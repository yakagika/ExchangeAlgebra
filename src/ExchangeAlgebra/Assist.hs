{-# LANGUAGE OverloadedStrings #-}
{- |Compatibility import for "ExchangeAlgebra.IO.Input.Assist".
-}
module ExchangeAlgebra.Assist {-# DEPRECATED "Import ExchangeAlgebra.IO.Input.Assist instead." #-}
    ( AccountInfo(..)
    , describeAccount
    , allAccountInfos
    , suggestAccounts
    , explainEntryError
    , explainJournalErrors
    , explainSourceErrors
    ) where

import ExchangeAlgebra.IO.Input.Assist
