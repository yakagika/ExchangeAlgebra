{- |
    Module     : ExchangeAlgebra.Render.Csv
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    CSV serialisation of text tables: 'writeCSV' (every cell quoted) and
    'csvTranspose' (transpose with blank padding). Both names are
    re-exported unchanged from "ExchangeAlgebra.Write"; this module only
    separates the file format from the bookkeeping layouts
    ("ExchangeAlgebra.Render.Bookkeeping") and the simulation dumps
    ("ExchangeAlgebra.Render.Simulation").

    >>> import qualified Data.Text as T
    >>> csvTranspose [[T.pack "a", T.pack "b"], [T.pack "c"]]
    [["a","c"],["b",""]]
-}

module ExchangeAlgebra.Render.Csv
    ( writeCSV
    , csvTranspose
    ) where

import           ExchangeAlgebra.Write (writeCSV, csvTranspose)
