{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

-- | Account title metadata and direction helpers for exchange bases (Definition 7).
module ExchangeAlgebra.Accounting.Account
    ( AccountTitles(..)
    , AccountDivision(..), Side(..), ClosingRule(..), FixedCurrent(..)
    , AccountRole(..), PostingCapability(..), DivisionSemantics(..)
    , HomeSideSemantics(..), ReportingEligibility(..)
    , AccountSpec(..), AccountSemantics(..), accountAliases, accountSpec
    , accountSemantics, accountSpecMap, concreteAccountTitles
    , classifyAccountContra, accountDescriptions, jcciAliases
    , ProcessingContext(..), postingAllowedIn, postingCapabilityFor
    , PIMO(..), switchSide, defaultSide, classifyAccountDivision
    , pimoFromDivision, pimoFlip
    ) where

import ExchangeAlgebra.Algebra.Base.Representation (customError)
import ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import ExchangeAlgebra.Accounting.Account.Classification
import ExchangeAlgebra.Accounting.Account.Registry
import ExchangeAlgebra.Accounting.Account.Aliases (jcciAliases)
import ExchangeAlgebra.Accounting.Account.PostingPolicy
import GHC.Stack (HasCallStack)
import Control.DeepSeq (NFData(..))

-- | Reverse the credit/debit side. Swaps Credit and Debit.
-- The wildcard Side is returned unchanged.
--
-- Complexity: O(1)
{-# INLINE switchSide #-}
switchSide :: Side -> Side
switchSide Credit = Debit
switchSide Debit  = Credit
switchSide Side   = Side

-- | Default (home) side of an account division before any contra reversal:
-- Assets\/Cost are debit-normal, Liability\/Equity\/Revenue are credit-normal.
-- The actual home side of a base is this, reversed when 'isContra' holds
-- (contract: @isContra b == (homeSide of b \/= defaultSide (whatDiv b))@).
--
-- Complexity: O(1)
{-# INLINE defaultSide #-}
defaultSide :: AccountDivision -> Side
defaultSide Assets    = Debit
defaultSide Cost      = Debit
defaultSide Liability = Credit
defaultSide Equity    = Credit
defaultSide Revenue   = Credit

-- | Classify an account title into an account division (Assets/Equity/Liability/Cost/Revenue).
--
-- Complexity: O(1)
{-# INLINE classifyAccountDivision #-}
classifyAccountDivision :: HasCallStack => AccountTitles -> AccountDivision
classifyAccountDivision AccountTitle = customError "this is wildcard AccountTitle"
classifyAccountDivision title =
    case accountSpec title of
        Just spec -> asDivision spec
        Nothing   -> customError "this is wildcard AccountTitle"

-- | PIMO direction. In Proposition 5.3.8 (Deguchi 2004, pp.89-91) PS, IN,
-- MS and OUT mean __plus stock, input, minus stock and output__ —
-- directions of exchange, not statement labels. The allowed exchange pairs
-- are exactly PS ⇔ IN, PS ⇔ MS, OUT ⇔ IN, OUT ⇔ MS (the 'AccountBase'
-- instance below). The earlier Haddock glossed these as "Product Stock \/
-- Income \/ Money Stock \/ Outflow"; that was naming drift from the
-- original and is kept only as a mnemonic.
data PIMO   = PS  -- ^ plus stock (stock increase; non-contra Assets)
            | IN  -- ^ input (flow in; Revenue)
            | MS  -- ^ minus stock (stock decrease; Liability\/Equity and contra assets)
            | OUT -- ^ output (flow out; Cost)
            deriving (Ord, Show, Eq)

instance NFData PIMO where
    rnf value = value `seq` ()

-- | The division-to-PIMO map of the standard interpretation (the @g@ of
-- Proposition 5.3.8 restricted to non-contra accounts): Assets are plus
-- stock, Liability\/Equity are minus stock, Cost is output, Revenue is
-- input. Contra accounts flip this via 'pimoFlip' (see 'whatPIMO').
--
-- Complexity: O(1)
{-# INLINE pimoFromDivision #-}
pimoFromDivision :: AccountDivision -> PIMO
pimoFromDivision Assets    = PS
pimoFromDivision Equity    = MS
pimoFromDivision Liability = MS
pimoFromDivision Cost      = OUT
pimoFromDivision Revenue   = IN

-- | Direction flip used for contra accounts: PS ↔ MS, IN ↔ OUT.
-- Self-inverse, and it preserves the exchange relation:
-- @x \<=\> y@ implies @pimoFlip x \<=\> pimoFlip y@.
--
-- Complexity: O(1)
{-# INLINE pimoFlip #-}
pimoFlip :: PIMO -> PIMO
pimoFlip PS  = MS
pimoFlip MS  = PS
pimoFlip IN  = OUT
pimoFlip OUT = IN
