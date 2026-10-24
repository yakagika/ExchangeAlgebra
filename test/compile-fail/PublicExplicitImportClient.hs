{-# LANGUAGE OverloadedStrings #-}

module PublicExplicitImportClient where

import Data.List.NonEmpty (NonEmpty(..))
import ExchangeAlgebra.IO.Input.Admission
    ( CatalogCall
    , CatalogCallOf(..)
    , EntityInput
    , EntityInputOf(..)
    , TxKey(..)
    , EntityId(..)
    , PeriodId(..)
    , TxId(..)
    )

-- | Explicit imports expose constructors through the parameterized types.
publicCatalogCall :: CatalogCall
publicCatalogCall = Consolidate (publicEntityInput :| []) (publicKey :| [])

-- | The unresolved alias retains the original constructor.
publicEntityInput :: EntityInput
publicEntityInput = EntityInput (EntityId "company") (publicKey :| [])

-- | Fixture key for the explicit-import client.
publicKey :: TxKey
publicKey = TxKey (EntityId "company") (PeriodId "2026") (TxId "source")
