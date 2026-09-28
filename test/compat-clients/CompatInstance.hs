{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module CompatInstance (checkInstances) where

import Control.Monad (unless)
import Data.Hashable (Hashable)
import ExchangeAlgebra.Algebra
    (Alg, Exchange(..), Redundant(..), (.@), (.+))
import ExchangeAlgebra.Algebra.Base
    ( AccountDivision(..), AccountTitles(..), BaseClass, Element(..)
    , ExBaseClass(..), FixedCurrent(..), Hat(..), HatBase((:<))
    , HatBaseClass(..), PIMO(..), Side(..))
import qualified ExchangeAlgebra.Journal as J

newtype DefaultBase = DefaultBase (HatBase AccountTitles)
    deriving (Eq, Ord, Show, Hashable)

instance Element DefaultBase where
    wildcard = DefaultBase wildcard

instance BaseClass DefaultBase

instance HatBaseClass DefaultBase where
    type BasePart DefaultBase = AccountTitles
    base (DefaultBase wrapped) = base wrapped
    hat (DefaultBase wrapped) = hat wrapped
    merge h title = DefaultBase (h :< title)
    toHat (DefaultBase wrapped) = DefaultBase (toHat wrapped)
    toNot (DefaultBase wrapped) = DefaultBase (toNot wrapped)
    revHat (DefaultBase wrapped) = DefaultBase (revHat wrapped)
    isHat (DefaultBase wrapped) = isHat wrapped
    isNot (DefaultBase wrapped) = isNot wrapped

instance ExBaseClass DefaultBase where
    getAccountTitle (DefaultBase (_ :< title)) = title
    setAccountTitle (DefaultBase (h :< _)) title = DefaultBase (h :< title)

newtype OverrideBase = OverrideBase (HatBase AccountTitles)
    deriving (Eq, Ord, Show, Hashable)

instance Element OverrideBase where
    wildcard = OverrideBase wildcard

instance BaseClass OverrideBase

instance HatBaseClass OverrideBase where
    type BasePart OverrideBase = AccountTitles
    base (OverrideBase wrapped) = base wrapped
    hat (OverrideBase wrapped) = hat wrapped
    merge h title = OverrideBase (h :< title)
    toHat (OverrideBase wrapped) = OverrideBase (toHat wrapped)
    toNot (OverrideBase wrapped) = OverrideBase (toNot wrapped)
    revHat (OverrideBase wrapped) = OverrideBase (revHat wrapped)
    isHat (OverrideBase wrapped) = isHat wrapped
    isNot (OverrideBase wrapped) = isNot wrapped

instance ExBaseClass OverrideBase where
    getAccountTitle (OverrideBase (_ :< title)) = title
    setAccountTitle (OverrideBase (h :< _)) title = OverrideBase (h :< title)
    whatDiv _ = Revenue
    isContra _ = True

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual =
    unless (expected == actual) (fail (label ++ ": " ++ show actual))

checkInstances :: IO ()
checkInstances = do
    let debit = DefaultBase (Not :< Cash)
        credit = DefaultBase (Hat :< Cash)
        alg = 4 .@ debit .+ 4 .@ credit :: Alg Double DefaultBase
        journal = alg J..| "tx" :: J.Journal String Double DefaultBase
        debitOnly = 4 .@ debit :: Alg Double DefaultBase
        debitJournal = debitOnly J..| "tx" :: J.Journal String Double DefaultBase
        override = OverrideBase (Not :< Cash)
    check "default division" Assets (whatDiv debit)
    check "default contra" False (isContra debit)
    check "default PIMO" PS (whatPIMO debit)
    check "default current" Current (fixedCurrent debit)
    check "default debit side" Debit (whichSide debit)
    check "default credit side" Credit (whichSide credit)
    check "override division" Revenue (whatDiv override)
    check "override contra" True (isContra override)
    check "override PIMO" OUT (whatPIMO override)
    check "override side" Debit (whichSide override)
    check "Alg decL" 4 (norm (decL alg))
    check "Alg decR" 4 (norm (decR alg))
    check "Alg balance" True (balance alg)
    check "Alg diffRL" (Side, 0) (diffRL alg)
    check "Alg debit-only balance" False (balance debitOnly)
    check "Alg debit-only diffRL" (Debit, 4) (diffRL debitOnly)
    check "Journal decL" 4 (norm (decL journal))
    check "Journal decR" 4 (norm (decR journal))
    check "Journal balance" True (balance journal)
    check "Journal diffRL" (Side, 0) (diffRL journal)
    check "Journal debit-only balance" False (balance debitJournal)
    check "Journal debit-only diffRL" (Debit, 4) (diffRL debitJournal)
    putStrLn "[PASS] user-defined instances and exchange results"
