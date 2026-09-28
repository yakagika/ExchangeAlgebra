{- | Generic basis elements and one-way wildcard matching (Definition 1).

Use 'matchesQuery' to match a query pattern against a stored element. The
wildcard in the query matches any stored value; a concrete query does not
match a stored wildcard.

== Extending with user-defined types

To use your own type as a basis component, declare an 'Element' instance.
A single distinguished value must serve as the wildcard used by the
transfer engine and by projection operations. Derive 'NFData' when keys
containing this component must be fully evaluated:

@
data Company = CompanyA | CompanyB | CompanyWildcard
  deriving (Eq, Ord, Show, Generic, Hashable, NFData, Typeable)

instance Element Company where
  wildcard = CompanyWildcard
@

== Import guidance

Import 'Element' and its associated symbols from
"ExchangeAlgebra.Algebra.Element". The old
"ExchangeAlgebra.Algebra.Base.Element" module is deprecated.
-}
module ExchangeAlgebra.Algebra.Element
    ( Element(..), AxisKey(..), axisIsWildcard, AxisDecompose(..), (.#)
    , Name, Subject, CountUnit(..), matchesQuery, Hashable(..), Generic
    ) where

import ExchangeAlgebra.Algebra.Element.Representation
