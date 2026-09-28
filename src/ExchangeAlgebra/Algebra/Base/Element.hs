-- | Import "ExchangeAlgebra.Algebra.Element" instead.
module ExchangeAlgebra.Algebra.Base.Element {-# DEPRECATED "Use ExchangeAlgebra.Algebra.Element instead." #-}
    ( Element(..), AxisKey(..), axisIsWildcard, AxisDecompose(..), (.#)
    , AccountTitles(..), Name, Subject, CountUnit(..), Hashable(..), Generic
    ) where

import ExchangeAlgebra.Algebra.Element.Representation
    ( Element(..), AxisKey(..), axisIsWildcard, AxisDecompose(..), (.#)
    , Name, Subject, CountUnit(..), Hashable(..), Generic )
import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
