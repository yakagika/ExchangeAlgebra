{- |
    Module     : ExchangeAlgebra.Journal
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Import "ExchangeAlgebra.Journal.Core" for generic journal operations and
    "ExchangeAlgebra.Accounting.Exchange" for accounting operations.

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>


-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}

module ExchangeAlgebra.Journal
    ( module ExchangeAlgebra.Algebra.Base
    , HatVal(..)
    , HatBaseClass(..)
    , Redundant(..)
    , Exchange(..)
    , pattern (:@)
    , (.@)
    , Note(..)
    , NoteAxisKey(..)
    , NoteAxisPosting
    , Journal
    , pattern ExchangeAlgebra.Journal.Zero
    , mkJournal
    , (.|)
    , toAlg
    , toMap
    , fromMap
    , fromList
    , sigma
    , sigma2When
    , sigmaOn
    , sigmaOnFromMap
    , decTo
    , sigmaM
    , map
    , replaceNotes
    , insert
    , projWithNote
    , projWithBase
    , projWithNoteBase
    , projWithBaseNetNorm
    , projWithNoteBaseNetNorm
    , projWithBaseNorm
    , projWithNoteNorm
    , filterWithNote
    , filterByAxis
    , gather
    ) where

import ExchangeAlgebra.Algebra.Base
import ExchangeAlgebra.Algebra.Value.Class (HatVal(..))
import ExchangeAlgebra.Algebra.Core.Representation (Redundant(..), pattern (:@), (.@))
import ExchangeAlgebra.Accounting.Exchange (Exchange(..))
import ExchangeAlgebra.Journal.Core.Representation as ExchangeAlgebra.Journal
    ( Note(..)
    , NoteAxisKey(..)
    , NoteAxisPosting
    , Journal
    , pattern Zero
    , mkJournal
    , (.|)
    , toAlg
    , toMap
    , fromMap
    , fromList
    , sigma
    , sigma2When
    , sigmaOn
    , sigmaOnFromMap
    , decTo
    , sigmaM
    , map
    , replaceNotes
    , insert
    , projWithNote
    , projWithBase
    , projWithNoteBase
    , projWithBaseNetNorm
    , projWithNoteBaseNetNorm
    , projWithBaseNorm
    , projWithNoteNorm
    , filterWithNote
    , filterByAxis
    , gather
    )
import Prelude hiding (map)
