{- |
    Module     : ExchangeAlgebra.Base
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping system.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DeriveGeneric              #-}


module ExchangeAlgebra.Algebra.Base
    ( module ExchangeAlgebra.Algebra.Base
    , module ExchangeAlgebra.Algebra.Base.Element) where

import ExchangeAlgebra.Algebra.Base.Element

import              Data.Time           (Day, TimeOfDay)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.Binary as Binary

customError :: HasCallStack => String -> a
customError msg = error (msg ++ "\nCallStack:\n" ++ prettyCallStack callStack)

------------------------------------------------------------------
-- * Base conditions
------------------------------------------------------------------

-- ** Base
------------------------------------------------------------------
{- | Base class definition.
    Any type that is an instance of this class qualifies as a base.
-}

class (Element a) =>  BaseClass a where
    compareBase :: a -> a -> Ordering
    compareBase = compareElement

instance (Element e1, Element e2)
        => BaseClass (e1, e2) where

instance (Element e1, Element e2, Element e3)
        => BaseClass (e1, e2, e3) where

instance (Element e1, Element e2, Element e3, Element e4)
        => BaseClass (e1, e2, e3, e4) where

instance (Element e1, Element e2, Element e3, Element e4, Element e5)
        => BaseClass (e1, e2, e3, e4, e5) where

instance (Element e1, Element e2, Element e3, Element e4, Element e5, Element e6)
        => BaseClass (e1, e2, e3, e4, e5, e6) where


------------------------------------------------------------------
-- ** HatBase
------------------------------------------------------------------

-- | Type class for bases with a Hat component. Provides functionality to decompose and
-- compose a base into its Hat part and BasePart. Manages the credit (Hat) / debit (Not)
-- distinction at the base level in exchange algebra.
class (BaseClass a, BaseClass (BasePart a), AxisDecompose (BasePart a)) => HatBaseClass a where
    -- | The type of the base part excluding the Hat.
    type BasePart a
    -- | Extract the base part excluding the Hat. Complexity: O(1)
    base    :: (BaseClass (BasePart a)) => a -> BasePart a
    -- | Extract the Hat part. Complexity: O(1)
    hat     :: a    -> Hat

    -- | Reconstruct a base from a Hat and a BasePart. Complexity: O(1)
    merge :: Hat -> BasePart a -> a

    -- | Convert to the Hat side. Complexity: O(1)
    toHat   :: a    -> a
    -- | Convert to the Not side. Complexity: O(1)
    toNot   :: a    -> a
    -- | Reverse Hat/Not. Complexity: O(1)
    revHat  :: a    -> a
    -- | Test whether the base is Hat. Complexity: O(1)
    isHat   :: a    -> Bool
    -- | Test whether the base is Not. Complexity: O(1)
    isNot   :: a    -> Bool

    -- | Compare bases with Hat. Defaults to 'compareBase'. Complexity: O(k)
    compareHatBase :: a -> a -> Ordering
    compareHatBase = compareBase

------------------------------------------------------------------
-- | Hat definition
data Hat    = Hat
            | Not
            | HatNot
            deriving (Enum, Eq, Ord, Show, Generic)

instance Hashable Hat where
instance Binary.Binary Hat

instance Element Hat where
    wiledcard = HatNot

    {-# INLINE equal #-}
    equal Hat Hat = True
    equal Hat Not = False
    equal Not Hat = False
    equal Not Not = True
    equal _   _   = True

instance BaseClass Hat where

data BaseForSingleHat = BaseForSingleHat
    deriving (Eq,Ord,Generic)

instance Show BaseForSingleHat where
    show _ = ""

instance Hashable BaseForSingleHat where
instance Binary.Binary BaseForSingleHat

instance Element BaseForSingleHat where
    wiledcard = BaseForSingleHat
    equal _ _ = True

instance BaseClass BaseForSingleHat where

instance HatBaseClass Hat where
    type BasePart Hat = BaseForSingleHat
    hat  = id
    base x = BaseForSingleHat

    merge Hat _ = Hat
    merge Not _ = Not

    {-# INLINE toHat #-}
    toHat _ = Hat

    {-# INLINE toNot #-}
    toNot _ = Not

    {-# INLINE revHat #-}
    revHat Hat = Not
    revHat Not = Hat

    {-# INLINE isHat #-}
    isHat  Hat = True
    isHat  Not = False

    {-# INLINE isNot #-}
    isNot  = not . isHat
------------------------------------------------------------------

-- | Base with Hat. Attaches a Hat (credit-increase) / Not (debit-increase) label to a base
-- element such as an account title. Use the constructor @(:<)@ as in @Hat :< Cash@.
data HatBase a where
     (:<)  :: (BaseClass a) => {_hat :: Hat,  _base :: a } -> HatBase a

instance (BaseClass a, Binary.Binary a) => Binary.Binary (HatBase a) where
    put (h :< b) = Binary.put h >> Binary.put b
    get = (:<) <$> Binary.get <*> Binary.get

instance Show (HatBase a) where
    show (h :< b) = show h ++ ":<" ++ show b

instance Eq (HatBase a) where
    {-# INLINE (==) #-}
    (==) (h1 :< b1) (h2 :< b2) = h1 == h2 && b1 == b2
    {-# INLINE (/=) #-}
    (/=) x y = not (x == y)

instance Ord (HatBase a) where
    {-# INLINE compare #-}
    compare (h :< b) (h' :< b') =
        case compare b b' of
            EQ -> compare h h'
            x  -> x

instance (BaseClass a) => Hashable (HatBase a) where
     hashWithSalt salt (h:<b) = salt `hashWithSalt` h
                                     `hashWithSalt` b

-- | Element (HatBase a)
--  haveWiledcard
-- >>> haveWiledcard (HatNot:<Amount :: HatBase CountUnit)
-- True
--
-- (.==)
-- >>> Not:<(Cash, Yen) == Not:<(Cash,(.#))
-- False
--
-- >>> Not:<(Cash, Yen) .== Not:<(Cash,(.#))
-- True
--
--  compareElement
-- >>> type Test = HatBase CountUnit
-- >>> compareHatBase (Not:<Amount :: Test) (Not:<(.#) :: Test)
-- EQ
--
-- ignoreWiledcard
-- >>> ignoreWiledcard (Not:<(Products,Yen)) (Hat:<(Products,Amount))
-- Hat:<(Products,Amount)
--
-- >>> ignoreWiledcard (Not:<(Products,Yen)) (Hat:<(Products,(.#)))
-- Hat:<(Products,Yen)
--
-- >>> ignoreWiledcard (Not:<(Cash,(.#))) (HatNot:<((.#),Amount))
-- Not:<(Cash,Amount)


instance (BaseClass a) => Element (HatBase a) where
    wiledcard = HatNot :<wiledcard

    haveWiledcard (h:<b)
        = isWiledcard h
       || haveWiledcard b

    {-# INLINE equal #-}
    equal (h1:<b1) (h2:<b2) = h1 .== h2 && b1 .== b2

    ignoreWiledcard (h1:<b1) (h2:<b2)
        = (ignoreWiledcard h1 h2) :< (ignoreWiledcard b1 b2)


    compareElement (h1:<b1) (h2:<b2)
        = case compareElement b1 b2 of
            EQ -> compareElement h1 h2
            x  -> x

instance (BaseClass a) => BaseClass (HatBase a) where

instance (BaseClass a, AxisDecompose a) => HatBaseClass (HatBase a) where
    type BasePart (HatBase a) = a

    hat  = _hat

    base = _base

    merge = (:<)

    {-# INLINE toHat #-}
    toHat (h:<b) = Hat:<b

    {-# INLINE toNot #-}
    toNot (h:<b) = Not:<b

    {-# INLINE revHat #-}
    revHat (Hat :< b) = Not :< b
    revHat (Not :< b) = Hat :< b

    {-# INLINE isHat #-}
    isHat  (Hat :< b)    = True
    isHat  (Not :< b)    = False
    isHat  (HatNot :< b) = customError "called HatNot"

    {-# INLINE isNot #-}
    isNot  = not . isHat

------------------------------------------------------------
-- * Define ExBase
------------------------------------------------------------

-- | Credit/Debit distinction. Credit is the credit side, Debit is the debit side.
-- Side is a wildcard.
data Side   = Credit -- ^ Credit side
            | Debit  -- ^ Debit side
            | Side   -- ^ Wildcard
            deriving (Ord, Show, Eq)

-- | Reverse the credit/debit side. Swaps Credit and Debit.
-- The wildcard Side is returned unchanged.
--
-- Complexity: O(1)
{-# INLINE switchSide #-}
switchSide :: Side -> Side
switchSide Credit = Debit
switchSide Debit  = Credit
switchSide Side   = Side

-- | Fixed/Current distinction. Used for classifying account titles as fixed or current.
data FixedCurrent   = Fixed   -- ^ Fixed
                    | Current -- ^ Current
                    | Other   -- ^ Other (expenses, revenues, etc.)
                    deriving (Show, Eq)

-- | Classify an account title into an account division (Assets/Equity/Liability/Cost/Revenue).
--
-- Complexity: O(1)
{-# INLINE classifyAccountDivision #-}
classifyAccountDivision :: HasCallStack => AccountTitles -> AccountDivision
classifyAccountDivision AccountTitle                 = customError "this is wiledcard AccountTitle"
classifyAccountDivision CapitalStock                 = Equity
classifyAccountDivision RetainedEarnings            = Equity
classifyAccountDivision LongTermLoansPayable        = Liability
classifyAccountDivision ShortTermLoansPayable       = Liability
classifyAccountDivision LoansPayable                = Liability
classifyAccountDivision ReserveForDepreciation      = Liability
classifyAccountDivision DepositPayable              = Liability
classifyAccountDivision LongTermNationalBondsPayable  = Liability
classifyAccountDivision ShortTermNationalBondsPayable = Liability
classifyAccountDivision ReserveDepositPayable       = Liability
classifyAccountDivision CentralBankNotePayable      = Liability
classifyAccountDivision Depreciation                = Cost
classifyAccountDivision SalesCost                   = Cost
classifyAccountDivision BusinessTrip                = Cost
classifyAccountDivision Commutation                 = Cost
classifyAccountDivision UtilitiesExpense            = Cost
classifyAccountDivision RentExpense                 = Cost
classifyAccountDivision AdvertisingExpense          = Cost
classifyAccountDivision DeliveryExpenses            = Cost
classifyAccountDivision SuppliesExpenses            = Cost
classifyAccountDivision MiscellaneousExpenses       = Cost
classifyAccountDivision WageExpenditure             = Cost
classifyAccountDivision InterestExpense             = Cost
classifyAccountDivision TaxesExpense                = Cost
classifyAccountDivision ConsumptionExpenditure      = Cost
classifyAccountDivision SubsidyExpense              = Cost
classifyAccountDivision CentralBankPaymentExpense   = Cost
classifyAccountDivision Purchases                   = Cost
classifyAccountDivision NetIncome                   = Cost
classifyAccountDivision ValueAdded                  = Revenue
classifyAccountDivision SubsidyIncome               = Revenue
classifyAccountDivision NationalBondInterestEarned  = Revenue
classifyAccountDivision DepositInterestEarned       = Revenue
classifyAccountDivision GrossProfit                 = Revenue
classifyAccountDivision OrdinaryProfit              = Revenue
classifyAccountDivision InterestEarned              = Revenue
classifyAccountDivision ReceiptFee                  = Revenue
classifyAccountDivision RentalIncome                = Revenue
classifyAccountDivision WageEarned                  = Revenue
classifyAccountDivision TaxesRevenue                = Revenue
classifyAccountDivision CentralBankPaymentIncome    = Revenue
classifyAccountDivision Sales                       = Revenue
classifyAccountDivision NetLoss                     = Revenue
classifyAccountDivision _                           = Assets

-- | BaseClass ⊃ HatBaseClass ⊃ ExBaseClass
--
-- Extended type class for bases that carry an account title.
-- Provides access to and modification of account titles, account divisions, PIMO classification,
-- credit/debit determination, and fixed/current classification.
class (HatBaseClass a) => ExBaseClass a where
    -- | Retrieve the account title from a base. Complexity: O(1)
    getAccountTitle :: a -> AccountTitles

    -- | Change the account title of a base. Complexity: O(1)
    setAccountTitle :: a -> AccountTitles -> a

    -- | Account title setter operator. An alias for @setAccountTitle@. Complexity: O(1)
    {-# INLINE (.~) #-}
    (.~) :: a -> AccountTitles -> a
    (.~) = setAccountTitle

    -- | Retrieve the account division (Assets/Equity/Liability/Cost/Revenue). Complexity: O(1)
    {-# INLINE whatDiv #-}
    whatDiv     :: a -> AccountDivision
    whatDiv = classifyAccountDivision . getAccountTitle

    -- | Retrieve the PIMO classification (PS/IN/MS/OUT). Complexity: O(1)
    {-# INLINE whatPIMO #-}
    whatPIMO    :: a -> PIMO
    whatPIMO x =
        case whatDiv x of
            Assets    -> PS
            Equity    -> MS
            Liability -> MS
            Cost      -> OUT
            Revenue   -> IN

    -- | Determine whether a base belongs to the Credit or Debit side.
    -- Takes the Hat/Not reversal into account. Complexity: O(1)
    {-# INLINE whichSide #-}
    whichSide   :: a -> Side
    whichSide x =
        let side = f (whatDiv x)
        in if hat x == Not then side else switchSide side
        where
            {-# INLINE f #-}
            f Assets    = Credit
            f Cost      = Credit
            f Liability = Debit
            f Equity    = Debit
            f Revenue   = Debit

    -- credit :: [a] -- ^ Use projCredit when Elem contains Text, Int, etc.
    -- credit = L.filter (\x -> whichSide x == Credit) [toEnum 0 ..]

    -- debit :: [a] -- ^ Use projDebit when Elem contains Text, Int, etc.
    -- debit = L.filter (\x -> whichSide x == Debit) [toEnum 0 ..]

    -- | Retrieve the fixed/current classification.
    -- Returns Current, Fixed, or Other based on the account title.
    --
    -- Complexity: O(1)
    {-# INLINE fixedCurrent #-}
    fixedCurrent :: a -> FixedCurrent
    fixedCurrent b = f (getAccountTitle b)
        where
        {-# INLINE f #-}
        f Cash                           = Current
        f Deposits                       = Current
        f CurrentDeposits                = Current
        f Securities                     = Current
        f InvestmentSecurities           = Fixed
        f LongTermNationalBonds          = Fixed
        f ShortTermNationalBonds         = Current
        f Products                       = Current
        f Machinery                      = Fixed
        f Building                       = Fixed
        f Vehicle                        = Fixed
        f StockInvestment                = Other  -- Note
        f EquipmentInvestment            = Fixed
        f LongTermLoansReceivable        = Fixed
        f ShortTermLoansReceivable       = Current
        f ReserveDepositReceivable       = Current
        f Gold                           = Fixed
        f GovernmentService              = Current
        f CapitalStock                   = Other
        f RetainedEarnings               = Other
        f ShortTermLoansPayable          = Current
        f LoansPayable                   = Current
        f LongTermLoansPayable           = Fixed
        f ReserveForDepreciation         = Current
        f DepositPayable                 = Current
        f LongTermNationalBondsPayable   = Fixed
        f ShortTermNationalBondsPayable  = Current
        f ReserveDepositPayable          = Current
        f CentralBankNotePayable         = Current
        f Depreciation                   = Other
        f SalesCost                      = Other
        f BusinessTrip                   = Other
        f Commutation                    = Other
        f UtilitiesExpense               = Other
        f RentExpense                    = Other
        f AdvertisingExpense             = Other
        f DeliveryExpenses               = Other
        f SuppliesExpenses               = Other
        f MiscellaneousExpenses          = Other
        f WageExpenditure                = Other
        f InterestExpense                = Other
        f TaxesExpense                   = Other
        f ConsumptionExpenditure         = Other
        f SubsidyExpense                 = Other
        f CentralBankPaymentExpense      = Other
        f Purchases                      = Other
        f NetIncome                      = Other
        f ValueAdded                     = Other
        f SubsidyIncome                  = Other
        f NationalBondInterestEarned     = Other
        f DepositInterestEarned          = Other
        f GrossProfit                    = Other
        f OrdinaryProfit                 = Other
        f InterestEarned                 = Other
        f ReceiptFee                     = Other
        f RentalIncome                   = Other
        f WageEarned                     = Other
        f TaxesRevenue                   = Other
        f CentralBankPaymentIncome       = Other
        f NetLoss                        = Other
        f AccountTitle                   = Other


-- | Type class for determining correspondences between account divisions.
-- Tests whether two account divisions form a pair in double-entry bookkeeping
-- (e.g., Assets <=> Liability).
--
-- Complexity: O(1)
class AccountBase a where
    -- | Test whether two account divisions are in a corresponding relationship.
    (<=>) :: a -> a -> Bool

data AccountDivision = Assets       -- ^ Assets
                     | Equity       -- ^ Equity
                     | Liability    -- ^ Liability
                     | Cost         -- ^ Cost
                     | Revenue      -- ^ Revenue
                     deriving (Ord, Show, Eq)

instance AccountBase AccountDivision where
    Assets      <=> Liability       = True
    Liability   <=> Assets          = True
    Assets      <=> Equity          = True
    Equity      <=> Assets          = True
    Cost        <=> Liability       = True
    Liability   <=> Cost            = True
    Cost        <=> Equity          = True
    Equity      <=> Cost            = True
    _ <=> _ = False

-- | PIMO classification. Categories in exchange algebra: Product Stock (PS), Income (IN),
-- Money Stock (MS), and Outflow (OUT).
data PIMO   = PS  -- ^ Product Stock (Assets: production stock)
            | IN  -- ^ Income (Revenue: income flow)
            | MS  -- ^ Money Stock (Liability/Equity: monetary stock)
            | OUT -- ^ Outflow (Cost: expenditure flow)
            deriving (Ord, Show, Eq)

instance AccountBase PIMO where
    PS  <=> IN   = True
    IN  <=> PS   = True
    PS  <=> MS   = True
    MS  <=> PS   = True
    IN  <=> OUT  = True
    OUT <=> IN   = True
    MS  <=> OUT  = True
    OUT <=> MS   = True
    _   <=> _    = False


------------------------------------------------------------------
-- * Simple bases (can be extended as needed)
-- Tuples are used so that the same accessor functions can be shared.
-- This approach was chosen over the DuplicateRecordFields extension
-- because it has fewer restrictions and looks cleaner.
------------------------------------------------------------------

-- ** 1-element bases
-- *** Account title only (exchange algebra base)
instance BaseClass AccountTitles where

instance ExBaseClass (HatBase AccountTitles) where
    getAccountTitle (h :< a)   = a
    setAccountTitle (h :< a) b = h :< b

-- *** Name only (redundant algebra base)
instance BaseClass Name where

-- *** CountUnit only (redundant algebra base)
instance BaseClass CountUnit where

-- *** Day only (redundant algebra base)
instance BaseClass Day where

-- *** TimeOfDay only (redundant algebra base)
instance BaseClass TimeOfDay where

-- ***


-- ** 2-element bases

-- | Basic BaseClass with 2 elements

instance ExBaseClass (HatBase (AccountTitles, Day)) where
    getAccountTitle (h:< (a, d))   = a
    setAccountTitle (h:< (a, d)) b = h:< (b, d)

instance ExBaseClass (HatBase (AccountTitles, Name)) where
    getAccountTitle (h:< (a, n))   = a
    setAccountTitle (h:< (a, n)) b = h:< (b, n)

instance ExBaseClass (HatBase (CountUnit, AccountTitles)) where
    getAccountTitle (h:< (u, a))   = a
    setAccountTitle (h:< (u, a)) b = h:< (u, b)

-- ** 3-element bases
-- | Basic BaseClass with 3 elements
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit)) where
    getAccountTitle (h:< (a, n, c))   = a
    setAccountTitle (h:< (a, n, c)) b = h:< (b, n, c)

-- ** 4-element bases
-- | Basic BaseClass with 4 elements
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject)) where
    getAccountTitle (h:< (a, n, c, s))   = a
    setAccountTitle (h:< (a, n, c, s)) b = h:< (b, n, c, s)

-- ** 5-element bases
-- | Basic BaseClass with 5 elements
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject,  Day)) where
    getAccountTitle (h:< (a, n, c, s, d))   = a
    setAccountTitle (h:< (a, n, c, s, d)) b = h:< (b, n, c, s, d)


-- ** 6-element bases
-- | Basic BaseClass with 6 elements
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject, Day, TimeOfDay)) where
    getAccountTitle (h:< (a, n, c, s, d, t))   = a
    setAccountTitle (h:< (a, n, c, s, d, t)) b = h:< (b, n, c, s, d, t)
