{- |
    Module     : ExchangeAlgebra.Accounting
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    The accounting layer on top of "ExchangeAlgebra.Foundation": the names
    that realise Definitions 7-9 of Akagi (2026), Appendix A.

    * Definition 7 (exchange base class): account titles, account divisions,
      sides, and 'ExBaseClass', whose 'whichSide' places a posting on its
      home side for 'Not' and on the opposite side for 'Hat' (the L\/R
      convention follows Deguchi & Nakano 1986, Definition 2.16: 'decL' is
      the debit part, 'decR' the credit part).
    * Definition 8 (exchange algebra): the 'Exchange' decomposition
      ('decL' \/ 'decR' \/ 'decP' \/ 'decM') and the account-aware projections.
    * Definition 9 (transfer): the 'TransTable' triple and 'transfer'.

    Everything is re-exported item by item from
    "ExchangeAlgebra.Algebra.Base.Element", "ExchangeAlgebra.Algebra.Base.Account.Types",
    "ExchangeAlgebra.Algebra.Base", "ExchangeAlgebra.Algebra.Internal" and
    "ExchangeAlgebra.Algebra.Transfer"; nothing is defined here. The umbrella
    is the Definition 7-9 core only: the account registry
    ("ExchangeAlgebra.Algebra.Base.Account.Registry"), the posting policy
    ("ExchangeAlgebra.Accounting.PostingPolicy"), trial-balance readouts
    ("ExchangeAlgebra.TrialBalance.Balance") and the named closing transfers
    of "ExchangeAlgebra.Algebra.Transfer" keep their own modules.

    >>> import ExchangeAlgebra.Foundation
    >>> import ExchangeAlgebra.Accounting
    >>> let x = 100 .@ Not :< Cash .+ 100 .@ Not :< Sales :: Alg Double (HatBase AccountTitles)
    >>> (norm (decL x), norm (decR x), balance x)
    (100.0,100.0,True)
-}

module ExchangeAlgebra.Accounting
    ( -- * Definition 7: exchange base class
      AccountTitles(..)
    , AccountDivision(..)
    , Side(..)
    , FixedCurrent(..)
    , PIMO(..)
    , ExBaseClass(..)
    , AccountBase(..)
    , switchSide
    , defaultSide
    , classifyAccountDivision
    , pimoFromDivision
    , pimoFlip
      -- * Definition 8: exchange algebra
    , Exchange(..)
    , projCredit
    , projDebit
    , projByAccountTitle
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , projContraAssets
    , projContra
      -- * Definition 9: transfer
    , TransTable
    , TransTableParts
    , Size
    , isNullTable
    , table
    , (.->)
    , (|%)
    , createTransfer
    , transfer
    ) where

import           ExchangeAlgebra.Algebra.Base.Element (AccountTitles(..))
import           ExchangeAlgebra.Algebra.Base.Account.Types
                     ( AccountDivision(..)
                     , Side(..)
                     , FixedCurrent(..)
                     )
import           ExchangeAlgebra.Algebra.Base
                     ( PIMO(..)
                     , ExBaseClass(..)
                     , AccountBase(..)
                     , switchSide
                     , defaultSide
                     , classifyAccountDivision
                     , pimoFromDivision
                     , pimoFlip
                     )
import           ExchangeAlgebra.Algebra.Internal
                     ( Exchange(..)
                     , projCredit
                     , projDebit
                     , projByAccountTitle
                     , projCurrentAssets
                     , projFixedAssets
                     , projDeferredAssets
                     , projCurrentLiability
                     , projFixedLiability
                     , projCapitalStock
                     , projContraAssets
                     , projContra
                     )
import           ExchangeAlgebra.Algebra.Transfer
                     ( TransTable
                     , TransTableParts
                     , Size
                     , isNullTable
                     , table
                     , (.->)
                     , (|%)
                     , createTransfer
                     , transfer
                     )
