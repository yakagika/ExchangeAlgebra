{- | Elements of element sets and one-way wildcard matching (Definition 1).

Use 'matchesQuery' to match a query pattern against a stored element. The
wildcard in the query matches any stored value; a concrete query does not
match a stored wildcard.

== Extending with user-defined types

To use your own type as an element type (the type of one component of a base), declare an 'Element' instance.
A single distinguished value must serve as the wildcard used by the
transfer engine and by projection operations. Derive t'Control.DeepSeq.NFData' when keys
containing this component must be fully evaluated:

@
data Company = CompanyA | CompanyB | CompanyWildcard
  deriving (Eq, Ord, Show, Generic, Hashable, NFData, Typeable)

instance Element Company where
  wildcard = CompanyWildcard
@

Use @Wild Day@ and @Wild TimeOfDay@ for date and time elements in a base.
Wrap a concrete date or time in 'Exactly' and use the v'Wild' constructor
as the wildcard. You can also wrap a custom identifier whose type has
no 'Element' instance.

>>> import Data.Time (TimeOfDay, midnight)
>>> matchesQuery (Exactly midnight) (Wild :: Wild TimeOfDay)
False
>>> matchesQuery Wild (Exactly midnight)
True

== Import guidance

Import 'Element' and its associated symbols from
"ExchangeAlgebra.Algebra.Element".
-}
module ExchangeAlgebra.Algebra.Element
    ( Element(..), ElementKey(..), elementKeyIsWildcard, HasElementKeys(..), (.#)
    , Name, Subject, CountUnit(..), Wild(..), matchesQuery, Hashable(..), Generic
    ) where

import ExchangeAlgebra.Algebra.Element.Representation
