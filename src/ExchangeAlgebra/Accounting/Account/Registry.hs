-- | Public metadata and alias projections from the account title table.
module ExchangeAlgebra.Accounting.Account.Registry
    ( AccountSpec(..)
    , AccountSemantics(..)
    , accountAliases
    , jcciAliases
    , accountSpec
    , accountSemantics
    , accountSpecMap
    , concreteAccountTitles
    , classifyAccountContra
    , accountDescriptions
    ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)

import ExchangeAlgebra.Accounting.Account.Spec (AccountSpec(..), AccountSemantics(..))
import ExchangeAlgebra.Accounting.Account.Title (AccountTitles, Row(..), row)

-- | All concrete account titles in their stable Enum order.
concreteAccountTitles :: [AccountTitles]
concreteAccountTitles =
    [title | title <- [minBound .. maxBound], ConcreteRow {} <- [row title]]

-- | Look up processing and reporting semantics for a concrete account title.
-- The wildcard returns 'Nothing'.
accountSemantics :: AccountTitles -> Maybe AccountSemantics
accountSemantics title = case row title of
    WildcardRow                              -> Nothing
    ConcreteRow { rowSemantics = semantics } -> Just semantics

-- | Every accepted non-canonical alias for an account. Base aliases come first,
-- followed by JCCI 2022 aliases, with duplicates removed.
accountAliases :: AccountTitles -> [Text]
accountAliases title = L.nub (baseAliases <> jcciAliases title)
  where
    baseAliases = maybe [] asAliases (accountSpec title)

-- | Return the JCCI 2022 account-name aliases for a title.
jcciAliases :: AccountTitles -> [Text]
jcciAliases title = case row title of
    WildcardRow                              -> []
    ConcreteRow { rowJcciAliases = aliases } -> aliases

-- | Look up metadata for an account title. The wildcard returns 'Nothing'.
--
-- Complexity: O(1)
{-# INLINE accountSpec #-}
accountSpec :: AccountTitles -> Maybe AccountSpec
accountSpec title = case row title of
    WildcardRow                    -> Nothing
    ConcreteRow { rowSpec = spec } -> Just spec

-- | A strict map of the metadata for all concrete account titles.
accountSpecMap :: Map AccountTitles AccountSpec
accountSpecMap = M.fromList
    [ (title, spec)
    | title <- concreteAccountTitles
    , Just spec <- [accountSpec title]
    ]

-- | Whether an account is a contra account.
--
-- The result is projected from 'AccountSpec.asIsContra'; the wildcard has no
-- specification and therefore returns 'False'. Complexity: O(1)
{-# INLINE classifyAccountContra #-}
classifyAccountContra :: AccountTitles -> Bool
classifyAccountContra title = maybe False asIsContra (accountSpec title)

-- | Compatibility projection used by the assistance API.
accountDescriptions :: [(AccountTitles, Text, Text, Text)]
accountDescriptions = mapMaybe project concreteAccountTitles
  where
    project title = do
        spec <- accountSpec title
        pure (title, asNameEn spec, asNameJa spec, asDescription spec)
