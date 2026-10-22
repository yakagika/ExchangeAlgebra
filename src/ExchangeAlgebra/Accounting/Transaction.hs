{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Exact transaction coordinates and posting entries in the Accounting layer.
-- These types use algebra values and Journal notes; admission and equivalence
-- share them without a dependency on input validation. Read the identifiers
-- before 'Entry' and 'isBlankKey'.
--
-- The following names identify transactions and their supporting records.
--
-- +-----------------------------+---------------------------------------------+
-- | Task                        | Type or function                            |
-- +=============================+=============================================+
-- | Identify a transaction      | t'EntityId', t'PeriodId', t'TxId', t'TxKey' |
-- +-----------------------------+---------------------------------------------+
-- | Identify evidence or a call | t'FactId', t'EvidenceId', t'CallId'         |
-- +-----------------------------+---------------------------------------------+
-- | Keep original postings      | 'Entry'                                     |
-- +-----------------------------+---------------------------------------------+
-- | Check blank key coordinates | 'isBlankKey'                                |
-- +-----------------------------+---------------------------------------------+
--
-- > import qualified ExchangeAlgebra.Accounting.Transaction as Transaction
--
-- t'TxKey' supplies the note coordinates used by Definitions 10-12.
-- Constructing an identifier or an 'Entry' does not establish admission.
module ExchangeAlgebra.Accounting.Transaction
    ( EntityId(..)
    , PeriodId(..)
    , TxId(..)
    , FactId(..)
    , EvidenceId(..)
    , CallId(..)
    , TxKey(..)
    , Entry
    , isBlankKey
    ) where

import Data.Hashable (Hashable(..))
import Data.Text (Text)
import qualified Data.Text as Text

import ExchangeAlgebra.Accounting.Account (AccountTitles)
import ExchangeAlgebra.Algebra.Core (Alg)
import ExchangeAlgebra.Algebra.Base.Representation (HatBase)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import ExchangeAlgebra.Journal.Core (Note(..))

-- | Exact entity identity; admission rejects blank identifiers.
newtype EntityId = EntityId Text deriving (Eq, Ord, Show)

-- | Exact reporting-period identity; no implicit current period is assumed.
-- 'Ord' is the text order, not a calendar order: "2026-9" sorts after
-- "2026-10". Use it only for map keys; operations that need the order of
-- periods take the earlier and later period as explicit arguments.
newtype PeriodId = PeriodId Text deriving (Eq, Ord, Show)

-- | Transaction identity within one entity and period.
newtype TxId = TxId Text deriving (Eq, Ord, Show)

-- | Identity of a trusted entry in the specification's fact store.
newtype FactId = FactId Text deriving (Eq, Ord, Show)

-- | Identity of evidence stating a transaction's total debit amount.
newtype EvidenceId = EvidenceId Text deriving (Eq, Ord, Show)

-- | Invocation identity, independent of a generated transaction key.
newtype CallId = CallId Text deriving (Eq, Ord, Show)

-- | Hash each identifier by its exact text, as 'TxKey' hashes its coordinates.
instance Hashable EntityId where
    hashWithSalt salt (EntityId text) = hashWithSalt salt text

instance Hashable PeriodId where
    hashWithSalt salt (PeriodId text) = hashWithSalt salt text

instance Hashable TxId where
    hashWithSalt salt (TxId text) = hashWithSalt salt text

instance Hashable FactId where
    hashWithSalt salt (FactId text) = hashWithSalt salt text

instance Hashable EvidenceId where
    hashWithSalt salt (EvidenceId text) = hashWithSalt salt text

instance Hashable CallId where
    hashWithSalt salt (CallId text) = hashWithSalt salt text

-- | Exact transaction coordinate. Equality never performs wildcard matching.
data TxKey = TxKey EntityId PeriodId TxId -- ^ Entity, period, and transaction identity.
    deriving (Eq, Ord, Show)

-- | Hash the three exact text coordinates in their declared order.
instance Hashable TxKey where
    hashWithSalt salt (TxKey (EntityId entity) (PeriodId period) (TxId transaction)) =
        hashWithSalt salt (entity, period, transaction)

-- | Journal notes preserve transaction boundaries. The blank sentinel is
-- rejected by admission and is used only by the existing Journal interface.
instance Note TxKey where
    plank = TxKey (EntityId "") (PeriodId "") (TxId "")

-- | Concrete entry representation, retaining every original posting.
type Entry = Alg MoneyDecimal (HatBase AccountTitles)

-- | Reject empty or whitespace-only entity, period, or transaction identities.
isBlankKey :: TxKey -> Bool
isBlankKey (TxKey (EntityId entity) (PeriodId period) (TxId transaction)) =
    any (Text.null . Text.strip) [entity, period, transaction]

