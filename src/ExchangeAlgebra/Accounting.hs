{- |
    Module     : ExchangeAlgebra.Accounting
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    The accounting layer on top of "ExchangeAlgebra.Foundation": the names
    that realize Definitions 7-9 of Akagi (2026), Appendix A.

    * Definition 7 (exchange base class): account titles, account divisions,
      sides, and 'ExBaseClass', whose 'whichSide' places a posting on its
      home side for 'ExchangeAlgebra.Algebra.Base.Not' and on the opposite side for
      v'ExchangeAlgebra.Algebra.Base.Hat' (the L\/R
      convention follows Deguchi & Nakano 1986, Definition 2.16: 'decL' is
      the debit part, 'decR' the credit part).
    * Definition 8 (exchange algebra): the 'Exchange' decomposition
      ('decL' \/ 'decR' \/ 'decP' \/ 'decM') and the account-aware projections.
    * Definition 9 (transfer): the accounting transfer of
      "ExchangeAlgebra.Accounting.Transfer" (tables written with '.->' and
      '|%', additive 'transferEntries' from exact balances), plus coordinate
      rules and closing entries. Import generic per-posting @transferEntries@
      and @Relabel@ from "ExchangeAlgebra.Algebra.Transfer" qualified.

    This umbrella re-exports account vocabulary, exchange operations,
    coordinate rules, closing entries, and accounting transfers. It defines
    no operations. The account registry ("ExchangeAlgebra.Accounting.Account"),
    trial-balance readouts ("ExchangeAlgebra.Accounting.TrialBalance.Balance"),
    and statement modules are imported separately.

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
      -- * Definition 9: coordinate rules and closing
    , module ExchangeAlgebra.Algebra.Transfer.Rule
      -- * Definition 9: accounting transfer
    , module ExchangeAlgebra.Accounting.Transfer
    ) where

import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import           ExchangeAlgebra.Accounting.Account.Classification
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
import           ExchangeAlgebra.Accounting.Transfer

import ExchangeAlgebra.Algebra.Transfer.Representation as ExchangeAlgebra.Algebra.Transfer.Rule
    ( TransferScale(MulBy, DivBy)
    , TransferRule(..)
    , TransferRules
    , TransferRuleError(..)
    , TransferApplyError(..)
    , mkTransferRules
    , rulesToList
    , relabel
    , scaleBy
    , divideBy
    , collapseEntries
    , collapseNetEntries
    )

import ExchangeAlgebra.Accounting.Closing as ExchangeAlgebra.Algebra.Transfer.Rule
    ( ClosingSide(..)
    , closingSide
    , closingEntries
    , SettleRule
    , retainedEarningsRule
    , SettlementBatch
    , SignedNet
    , SettleError(..)
    , settleEntries
    , settlementSteps
    )
