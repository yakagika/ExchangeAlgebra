{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{- |Compatibility import for "ExchangeAlgebra.IO.Input" and
"ExchangeAlgebra.Accounting.Account".
-}
module ExchangeAlgebra.Convert {-# DEPRECATED "Import ExchangeAlgebra.IO.Input or ExchangeAlgebra.Accounting.Account for concreteAccountTitles instead." #-}
    ( ConvError(..)
      -- $concreteAccountTitles
    , concreteAccountTitles
    , normalizeTitle
    , parseAccountTitle
    , parseSide
    , markerForSide
    , postingFromSide
    , journalFromSides
    ) where

import ExchangeAlgebra.IO.Input
import ExchangeAlgebra.Accounting.Account (concreteAccountTitles)
