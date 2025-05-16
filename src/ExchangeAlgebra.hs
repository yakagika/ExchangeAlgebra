

{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

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