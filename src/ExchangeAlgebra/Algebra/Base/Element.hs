{- |
    Module     : ExchangeAlgebra.Algebra.Base.Element
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    == Extending with user-defined types

    To use your own type as a basis component, declare an 'Element' instance.
    A single distinguished value must serve as the wildcard used by the
    transfer engine and by projection operations:

    @
    data Company = CompanyA | CompanyB | CompanyWildcard
      deriving (Eq, Ord, Show, Generic, Hashable, Typeable)

    instance Element Company where
      wiledcard = CompanyWildcard
    @

    == Import guidance

    User code rarely needs to import this module directly. The 'Element'
    class and its associated symbols are re-exported from
    "ExchangeAlgebra.Algebra.Base", and transitively from
    "ExchangeAlgebra.Algebra", "ExchangeAlgebra.Journal", and the top-level
    "ExchangeAlgebra". The module path of this file may be reorganized in
    future versions; prefer importing from the higher-level modules.

-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE DeriveGeneric              #-}

module ExchangeAlgebra.Algebra.Base.Element
    ( module ExchangeAlgebra.Algebra.Base.Element
    , module Data.Hashable
    , module GHC.Generics) where

import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.Time           as Time
import              Data.Time
import GHC.Generics (Generic)
import Data.Hashable
import Data.Typeable (Typeable, cast, typeOf)
import qualified Data.Binary as Binary

------------------------------------------------------------------
-- * Element (components of bases)
------------------------------------------------------------------

-- | Element Class: a type must be an instance of this class to serve as a component of a basis.
--
-- Each component of a basis must be an instance of this type class.
-- It provides wildcard-based pattern matching, enabling flexible basis
-- specification in transfer transformations and projections.
class (Eq a, Ord a, Show a, Hashable a, Typeable a) => Element a where

    -- | The wildcard value. Used for pattern matching in search, transfer transformation, etc.
    --
    -- Complexity: O(1)
    wiledcard       :: a

    -- | Determines whether the element itself or any of its internal components contains a wildcard.
    -- For tuple elements, returns True if any component is a wildcard.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE haveWiledcard #-}
    haveWiledcard :: a -> Bool
    haveWiledcard = isWiledcard

    -- | Determines whether the value is exactly the wildcard.
    --
    -- Complexity: O(1)
    {-# INLINE isWiledcard #-}
    isWiledcard     :: a -> Bool
    isWiledcard a = a == wiledcard

    -- | Wildcard-ignoring transformation.
    -- If @after@ is a wildcard, returns @before@.
    -- Used inside transfer to fill wildcard positions in the target basis with the original values.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard :: a -> a -> a
    ignoreWiledcard before after
        | before == after   = before
        | isWiledcard after = before
        | otherwise         = after

    -- | Wildcard-aware equality test.
    -- Returns True if either operand is a wildcard.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE equal #-}
    equal :: a -> a -> Bool
    equal a b | isWiledcard a = True
              | isWiledcard b = True
              | otherwise     = a == b

    -- | Equality operator that treats wildcards as equal.
    -- Unlike '==', @(.==)@ matches tuples that partially contain wildcards.
    --
    -- Complexity: O(k) (k is the number of tuple components; O(1) for primitive types)
    {-# INLINE (.==)  #-}
    (.==) :: a -> a -> Bool
    (.==) a b = a == b || (haveWiledcard a || haveWiledcard b) && equal a b

    -- | Inequality operator that treats wildcards as equal. Negation of @(.==)@.
    --
    -- Complexity: O(k)
    {-# INLINE (./=) #-}
    (./=) :: a -> a -> Bool
    (./=) a b = not (a .== b)

    -- | Wildcard-aware comparison.
    -- Returns EQ if the two values are equal under @(.==)@.
    --
    -- Complexity: O(k)
    {-# INLINE compareElement #-}
    compareElement :: a -> a -> Ordering
    compareElement x y
        | x .== y = EQ
        | otherwise = compare x y

    -- | Wildcard-aware less-than comparison.
    --
    -- Complexity: O(k)
    (.<) :: a -> a -> Bool
    (.<) x y = compareElement x y == LT

    -- | Wildcard-aware greater-than comparison.
    --
    -- Complexity: O(k)
    (.>) :: a -> a -> Bool
    (.>) x y = compareElement x y == GT

    -- | Wildcard-aware less-than-or-equal comparison.
    --
    -- Complexity: O(k)
    (.<=) :: a -> a -> Bool
    (.<=) x y = compareElement x y /= GT

    -- | Wildcard-aware greater-than-or-equal comparison.
    --
    -- Complexity: O(k)
    (.>=) :: a -> a -> Bool
    (.>=) x y = compareElement x y /= LT

    -- | Wildcard-aware maximum.
    --
    -- Complexity: O(k)
    maxElement :: a -> a -> a
    maxElement x y
        | x .>= y = x
        | otherwise = y

    -- | Wildcard-aware minimum.
    --
    -- Complexity: O(k)
    minElement :: a -> a -> a
    minElement x y
        | x .<= y = x
        | otherwise = y

-- | An existential type that holds each axis of a basis with its type erased.
-- Used to decompose multi-dimensional bases (tuples) into per-axis keys for indexing.
data AxisKey = forall a. Element a => AxisKey !a

instance Eq AxisKey where
    AxisKey x == AxisKey y = case cast y of
        Nothing -> False
        Just y' -> x == y'

instance Hashable AxisKey where
    hashWithSalt salt (AxisKey x) = salt `hashWithSalt` (typeOf x) `hashWithSalt` x

{-# INLINE axisIsWildcard #-}
axisIsWildcard :: AxisKey -> Bool
axisIsWildcard (AxisKey x) = isWiledcard x

-- | A type class for decomposing a basis element into a list of per-axis t'AxisKey's.
-- Overlapping instances are defined for tuple types so that each component
-- is decomposed into a separate t'AxisKey'.
--
-- Complexity: O(k) (k is the number of tuple components)
class (Element a) => AxisDecompose a where
    toAxisKeys :: a -> [AxisKey]

instance {-# OVERLAPPABLE #-} Element a => AxisDecompose a where
    {-# INLINE toAxisKeys #-}
    toAxisKeys a = [AxisKey a]

-- | Shorthand notation for the wildcard. An alias for @wiledcard@.
-- Write @(.#)@ when specifying patterns in projections and transfer transformations.
--
-- Complexity: O(1)
{-# INLINE (.#) #-}
(.#) :: Element a => a
(.#) = wiledcard

infix 4 .==
infix 4 ./=
------------------------------------------------------------------
-- * Elements
------------------------------------------------------------------

-- ** Account Titles

-- | The current version 0.1.0.0 will be completely changed shortly, especially this section.
data  AccountTitles = Cash                            -- ^ Asset: Cash
                    | Deposits                        -- ^ Asset: Savings deposits
                    | CurrentDeposits                 -- ^ Asset: Current deposits
                    | Securities                      -- ^ Asset: Securities
                    | InvestmentSecurities            -- ^ Asset: Investment securities
                    | LongTermNationalBonds           -- ^ Asset: Long-term national bonds
                    | ShortTermNationalBonds          -- ^ Asset: Short-term national bonds
                    | Products                        -- ^ Asset: Products
                    | Machinery                       -- ^ Asset: Machinery and equipment
                    | Building                        -- ^ Asset: Real estate
                    | Vehicle                         -- ^ Asset: Vehicles
                    | StockInvestment                 -- ^ Asset: Stock investment
                    | EquipmentInvestment             -- ^ Asset: Equipment investment
                    | LongTermLoansReceivable         -- ^ Asset: Loans receivable
                    | AccountsReceivable              -- ^ Asset: Accounts receivable
                    | ShortTermLoansReceivable        -- ^ Asset: Short-term loans receivable
                    | ReserveDepositReceivable        -- ^ Asset: Reserve deposit receivable
                    | Gold                            -- ^ Asset: Gold
                    | GovernmentService               -- ^ Asset: Government service expenditure
                    | CapitalStock                    -- ^ Equity: Capital stock
                    | RetainedEarnings                -- ^ Equity: Retained earnings
                    | LongTermLoansPayable            -- ^ Liability: Long-term loans payable
                    | ShortTermLoansPayable           -- ^ Liability: Short-term loans payable
                    | LoansPayable                    -- ^ Liability: Loans payable
                    | ReserveForDepreciation          -- ^ Liability: Reserve for depreciation
                    | DepositPayable                  -- ^ Liability: Deposits received
                    | LongTermNationalBondsPayable    -- ^ Liability: Long-term national bonds payable
                    | ShortTermNationalBondsPayable   -- ^ Liability: Short-term national bonds payable
                    | ReserveDepositPayable           -- ^ Liability: Accounts payable
                    | CentralBankNotePayable          -- ^ Liability: Central bank notes payable
                    | Depreciation                    -- ^ Expense: Depreciation
                    | SalesCost                       -- ^ Expense: Cost of sales
                    | BusinessTrip                    -- ^ Expense: Travel and transportation
                    | Commutation                     -- ^ Expense: Communication
                    | UtilitiesExpense                -- ^ Expense: Utilities
                    | RentExpense                     -- ^ Expense: Rent
                    | AdvertisingExpense              -- ^ Expense: Advertising
                    | DeliveryExpenses                -- ^ Expense: Delivery
                    | SuppliesExpenses                -- ^ Expense: Supplies
                    | MiscellaneousExpenses           -- ^ Expense: Miscellaneous
                    | WageExpenditure                 -- ^ Expense: Wages
                    | InterestExpense                 -- ^ Expense: Interest expense
                    | TaxesExpense                    -- ^ Expense: Taxes
                    | ConsumptionExpenditure          -- ^ Expense: Consumables
                    | SubsidyExpense                  -- ^ Expense: Subsidy expenditure
                    | CentralBankPaymentExpense       -- ^ Expense
                    | Purchases                       -- ^ Expense: Purchases
                    | NetIncome                       -- ^ Expense: Net income
                    | ValueAdded                      -- ^ Revenue: Value added
                    | SubsidyIncome                   -- ^ Revenue: Subsidy income
                    | NationalBondInterestEarned      -- ^ Revenue: National bond interest earned
                    | DepositInterestEarned           -- ^ Revenue: Deposit interest earned
                    | GrossProfit                     -- ^ Revenue: Gross profit
                    | OrdinaryProfit                  -- ^ Revenue: Ordinary profit
                    | InterestEarned                  -- ^ Revenue: Interest earned
                    | ReceiptFee                      -- ^ Revenue: Receipt fee
                    | RentalIncome                    -- ^ Revenue: Rental income
                    | WageEarned                      -- ^ Revenue: Wage income
                    | TaxesRevenue                    -- ^ Revenue: Tax revenue
                    | CentralBankPaymentIncome        -- ^ Revenue
                    | Sales                           -- ^ Revenue: Sales
                    | NetLoss                         -- ^ Revenue: Net loss
                    | AccountTitle                    -- ^ Wildcard
                    deriving (Show, Ord, Eq, Enum, Generic)

instance Hashable AccountTitles where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt x = hashWithSalt salt (fromEnum x)

instance Binary.Binary AccountTitles where
    {-# INLINE put #-}
    put = Binary.putWord8 . fromIntegral . fromEnum
    {-# INLINE get #-}
    get = toEnum . fromIntegral <$> Binary.getWord8

instance Element AccountTitles where
    {-# INLINE wiledcard #-}
    wiledcard = AccountTitle



-- | Name :: Name of an item
type Name = Text

-- | Subject of an account title
type Subject = Text
instance Element Text where

    {-# INLINE wiledcard #-}
    wiledcard   = T.empty

-- | Currency unit or physical quantity
data CountUnit  = Yen
                | Dollar
                | Euro
                | CNY
                | Amount
                | CountUnit
                deriving (Show, Ord, Eq, Enum,Generic)

instance Hashable CountUnit where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt x = hashWithSalt salt (fromEnum x)

instance Binary.Binary CountUnit where
    {-# INLINE put #-}
    put = Binary.putWord8 . fromIntegral . fromEnum
    {-# INLINE get #-}
    get = toEnum . fromIntegral <$> Binary.getWord8

instance Element CountUnit where

    {-# INLINE wiledcard #-}
    wiledcard = CountUnit


-- TimeOfDay internally holds hour, minute, and second (Pico), so each is hashed individually
instance Hashable TimeOfDay where
  hashWithSalt salt (TimeOfDay hour min sec) =
    salt `hashWithSalt` hour `hashWithSalt` min `hashWithSalt` sec

-- Day internally holds an Integer in ModifiedJulianDay format, so that is used for hashing
instance Hashable Day where
  hashWithSalt salt day = hashWithSalt salt (toModifiedJulianDay day)

instance Element TimeOfDay where
    wiledcard = Time.midnight

instance Element Day where
    wiledcard =  ModifiedJulianDay 0

instance (Element a ,Element b)
    => Element (a, b) where

    {-# INLINE wiledcard #-}
    wiledcard = (wiledcard, wiledcard)

    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b)
        = isWiledcard a
       || isWiledcard b

    {-# INLINE equal #-}
    equal (a1, a2) (b1, b2)
        =  (a1 .== b1)
        && (a2 .== b2)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2) (b1, b2)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2)

    {-# INLINE compareElement #-}
    compareElement (a1, a2) (b1, b2)
        = case compareElement a1 b1 of
            EQ -> compareElement a2 b2
            x  -> x

instance {-# OVERLAPPING #-} (Element a, Element b)
    => AxisDecompose (a, b) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b) = [AxisKey a, AxisKey b]

instance (Element a, Element b, Element c)
    => Element (a, b, c) where

    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c


    {-# INLINE equal #-}
    equal (a1, a2, a3) (b1, b2, b3)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3) (b1, b2, b3)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3) (b1, b2, b3)
        = compareElement ((a1, a2), a3)
                         ((b1, b2), b3)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c)
    => AxisDecompose (a, b, c) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c) = [AxisKey a, AxisKey b, AxisKey c]


instance (Element a, Element b, Element c, Element d)
    => Element (a, b, c, d) where

    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)


    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c,d)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c
       || isWiledcard d

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4) (b1, b2, b3, b4)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3, a4) (b1, b2, b3, b4)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3
          , ignoreWiledcard a4 b4)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4) (b1, b2, b3, b4)
        = compareElement ((a1, a2, a3), a4)
                         ((b1, b2, b3), b4)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d)
    => AxisDecompose (a, b, c, d) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d]


instance (Element a, Element b, Element c, Element d, Element e)
    => Element (a, b, c, d, e) where

    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)


    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c,d,e)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c
       || isWiledcard d
       || isWiledcard e

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3
          , ignoreWiledcard a4 b4
          , ignoreWiledcard a5 b5)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = compareElement ((a1, a2, a3, a4), a5)
                         ((b1, b2, b3, b4), b5)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e)
    => AxisDecompose (a, b, c, d, e) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e]


instance (Element a, Element b, Element c, Element d, Element e, Element f)
    => Element (a, b, c, d, e, f) where

    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c,d,e,f)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c
       || isWiledcard d
       || isWiledcard e
       || isWiledcard f

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3
          , ignoreWiledcard a4 b4
          , ignoreWiledcard a5 b5
          , ignoreWiledcard a6 b6)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = compareElement ((a1, a2, a3, a4, a5), a6)
                         ((b1, b2, b3, b4, b5), b6)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e, Element f)
    => AxisDecompose (a, b, c, d, e, f) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e, f) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e, AxisKey f]


instance (Element a, Element b, Element c, Element d, Element e, Element f, Element g)
    => Element (a, b, c, d, e, f, g) where
    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c,d,e,f,g)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c
       || isWiledcard d
       || isWiledcard e
       || isWiledcard f
       || isWiledcard g

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)
        && (a7 .== b7)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3
          , ignoreWiledcard a4 b4
          , ignoreWiledcard a5 b5
          , ignoreWiledcard a6 b6
          , ignoreWiledcard a7 b7)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = compareElement ((a1, a2, a3, a4, a5, a6), a7)
                         ((b1, b2, b3, b4, b5, b6), b7)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e, Element f, Element g)
    => AxisDecompose (a, b, c, d, e, f, g) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e, f, g) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e, AxisKey f, AxisKey g]
