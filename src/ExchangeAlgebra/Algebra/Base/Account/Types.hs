{- |
Module      : ExchangeAlgebra.Algebra.Base.Account.Types
Description : Account metadata types shared by Base and the account registry.
-}
module ExchangeAlgebra.Algebra.Base.Account.Types
    ( AccountDivision(..)
    , ClosingRule(..)
    , FixedCurrent(..)
    ) where

-- | Account division (financial-statement classification). The 'AccountBase'
-- correspondence instance lives in "ExchangeAlgebra.Algebra.Base" (the class's
-- home module), so this declaration stays instance-free.
data AccountDivision = Assets       -- ^ Assets
                     | Equity       -- ^ Equity
                     | Liability    -- ^ Liability
                     | Cost         -- ^ Cost
                     | Revenue      -- ^ Revenue
                     deriving (Ord, Show, Eq)

-- | Registry-level policy for automatic closing entries.
--
-- 'CloseByDivision' derives the transfer side from 'AccountDivision'.
-- 'NoClose' is an explicit override. Future policies may add explicit
-- keep/flip constructors without returning to an account-title case split.
data ClosingRule = CloseByDivision -- ^ Close Cost/Revenue accounts according to their division.
                 | NoClose         -- ^ Do not generate an automatic closing entry.
                 deriving (Show, Eq)

-- | Fixed/Current distinction. Used for classifying account titles as fixed or current.
data FixedCurrent = Fixed   -- ^ Fixed
                  | Current -- ^ Current
                  | Other   -- ^ Other (expenses, revenues, etc.)
                  deriving (Show, Eq)
