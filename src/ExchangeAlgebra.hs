

{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping systems.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    == Quick start

    This top-level module is the Algebra-layer umbrella: it re-exports
    "ExchangeAlgebra.Algebra", "ExchangeAlgebra.Algebra.Transfer",
    "ExchangeAlgebra.Write", and "ExchangeAlgebra.Simulate". It is the
    recommended entry point for simple single-period bookkeeping:

    @
    import ExchangeAlgebra

    -- A minimal exchange: 100 units of cash debited, 100 credited to sales.
    entry :: 'Alg' 'Double' ('HatBase' 'AccountTitles')
    entry = 100 ':\@' 'Hat' ':<' 'Cash' '.+' 100 ':\@' 'Not' ':<' 'Sales'
    @

    For multi-period simulation or metadata-aware journals (notes, axes),
    switch to the Journal layer. The two umbrellas export overlapping
    names (@sigma@, @fromList@, @map@, @filter@, …), so Journal-centric
    code should use qualified imports for the Algebra layer:

    @
    import           "ExchangeAlgebra.Journal"          -- umbrella + type classes
    import qualified "ExchangeAlgebra.Algebra"          as EA
    import qualified "ExchangeAlgebra.Journal"          as EJ
    import qualified "ExchangeAlgebra.Journal.Transfer" as EJT
    @

    == Full examples

    Runnable examples (elementary bookkeeping, ripple-effect simulations,
    a CGE model) live in the repository, not on Hackage:

    <https://github.com/yakagika/ExchangeAlgebra/tree/master/examples>

    See the repository's @README.md@ for installation and consumption
    patterns.

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}


module ExchangeAlgebra
    ( module ExchangeAlgebra.Algebra
    , module ExchangeAlgebra.Algebra.Transfer
    , module ExchangeAlgebra.Write
    , module ExchangeAlgebra.Simulate ) where

import              ExchangeAlgebra.Algebra
import              ExchangeAlgebra.Algebra.Transfer
import              ExchangeAlgebra.Write
import              ExchangeAlgebra.Simulate