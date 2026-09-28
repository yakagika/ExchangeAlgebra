{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{- |Compatibility import for "ExchangeAlgebra.IO.Input.Csv".
-}
module ExchangeAlgebra.Convert.Csv {-# DEPRECATED "Import ExchangeAlgebra.IO.Input.Csv instead." #-}
    ( -- * Parsing journal CSV
      parseJournalCsv
    , parseJournalCsvWith
      -- * Note-keyed journal (when the optional @note@ column is present)
    , parseNotedJournalCsv
      -- * Amount parsers
    , scientificAmount
      -- * Field splitting
    , splitTrim
    ) where

import ExchangeAlgebra.IO.Input.Csv
