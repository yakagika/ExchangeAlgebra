{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}

{- |
    Module     : ExchangeAlgebra.Value
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Exact, non-negative decimal value type t'MoneyDecimal' for use as the @v@
    parameter of @Alg v b@ / @Journal n v b@.

    == Why this exists (DESIGN, 2026-06-06)

    The accounting value type is selectable:

      * @Double@      — fast IEEE-754; the default. Addition is /non-associative/,
        so the order in which same-base postings are summed (which depends on how a
        value was /constructed/) can change @norm@ / @bar@ results. In the
        agent-based simulations this manifested as a ~3% swing in a stock value when
        the construction order changed (see plans/in-progress/LAZY_EVAL_AUDIT.md and
        FP_SUMMATION_SURVEY.md). This is acceptable for relative-price ABM work where
        speed matters, but it is not deterministic/auditable.

      * @MoneyDecimal@   — exact base-10 fixed-point (a non-negative 'Data.Decimal').
        Addition is /exact and associative/, so results are independent of
        construction order: the fromList fold direction, parallel merges, etc. no
        longer change the answer. This is the right choice for audited ledgers and
        for making the construction-order optimizations (fromList O(N)) safe.

    @Integer@ (minimal-currency-unit) is intentionally NOT offered: ABM simulations
    use relative prices with base unit 1 and sub-unit fractional prices, which an
    integer cannot represent.

    == Ergonomics

    Numeric literals work without wrapping, because 'Num'/'Fractional' are derived:

    > type Ledger = Journal Term MoneyDecimal (HatBase AccountTitles)
    > entry = 10.5 :@ Hat:<Cash .+ 2 :@ Not:<Sales   -- 10.5 and 2 are MoneyDecimal literals

    == Rounding

    The core algebra only adds/subtracts, which is exact for t'MoneyDecimal' and needs no
    rounding. Rounding is only needed by /multiplication and division/ (tax ratios,
    proration, scalar product) at the point a monetary amount is /finalized/. Use
    'bankersRound': it rounds half-to-even (the unbiased financial default; also GHC's
    'Prelude.round' and IEEE-754's default mode). A ceiling variant ('ceilingRound') is
    provided for the previous @rounding = ceiling@ behavior and for jurisdictions whose
    rules differ. There is no single correct rule (e.g. Japanese consumption tax rounding
    varies by company), so the rounding function is explicit and swappable.
-}
module ExchangeAlgebra.Value {-# DEPRECATED "Use ExchangeAlgebra.Algebra.Value instead." #-} 
    ( MoneyDecimal(..)
    , toDecimal
    , bankersRound
    , ceilingRound
    , MoneyParseError(..)
    , moneyDecimalFromText
    , moneyDecimalFromScientific
    , MoneyDouble(..)
    , toDouble
    ) where

import ExchangeAlgebra.Algebra.Value
    ( MoneyDecimal(..), toDecimal, bankersRound, ceilingRound
    , MoneyParseError(..), moneyDecimalFromText, moneyDecimalFromScientific
    , MoneyDouble(..), toDouble )
-- The old Algebra import keeps the existing instance-loading path until bc-exchange.
import ExchangeAlgebra.Algebra ()
