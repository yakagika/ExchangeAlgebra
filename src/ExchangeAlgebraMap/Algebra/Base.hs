{- |
    Module     : ExchangeAlgebra.Base
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}


module ExchangeAlgebraMap.Algebra.Base
    ( module ExchangeAlgebraMap.Algebra.Base
    , module ExchangeAlgebraMap.Algebra.Base.Element) where

import ExchangeAlgebraMap.Algebra.Base.Element

import qualified    Data.Time           as Time
import              Data.Time
import qualified    Data.List           as L (foldr1, map, length, elem,sort,foldl1,filter, or, and, sum)
import              Prelude             hiding (map, head, filter,tail, traverse, mapM)


------------------------------------------------------------------
-- * Base 基底の条件
------------------------------------------------------------------

-- ** Base
------------------------------------------------------------------
{- | 基底の定義
    これを継承すれば取り敢えず基底になれる
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
class (BaseClass a) => HatBaseClass a where
    toHat  :: a    -> a
    toNot  :: a    -> a
    hat    :: a    -> Hat
    revHat :: a    -> a     -- reverse Hat
    isHat  :: a    -> Bool
    isNot  :: a    -> Bool


    compareHatBase :: a -> a -> Ordering
    compareHatBase = compareBase

-- | Hat の定義
data Hat    = Hat
            | Not
            | HatNot
            deriving (Enum, Eq, Ord, Show)


instance Element Hat where
    wiledcard = HatNot

    {-# INLINE equal #-}
    equal Hat Hat = True
    equal Hat Not = False
    equal Not Hat = False
    equal Not Not = True
    equal _   _   = True

data HatBase a where
     (:<)  :: (BaseClass a) => {_hat :: Hat,  _base :: a } -> HatBase a

instance Eq (HatBase a) where
    {-# INLINE (==) #-}
    (==) (h1 :< b1) (h2 :< b2) = h1 == h2 && b1 == b2
    {-# INLINE (/=) #-}
    (/=) x y = not (x == y)

instance Ord (HatBase a) where
    compare (h :< b) (h' :< b')
        | b == b' = compare h h'
        | b >  b'  = GT
        | b <  b'  = LT

    (<) x y | compare x y == LT = True
            | otherwise         = False

    (>) x y | compare x y == GT = True
            | otherwise         = False


    (<=) x y | compare x y == LT || compare x y == EQ   = True
             | otherwise                                = False

    (>=) x y | compare x y == GT || compare x y == EQ   = True
             | otherwise                                = False

    max x y | x >= y    = x
            | otherwise = y

    min x y | x <= y    = x
            | otherwise = y

instance Show (HatBase a) where
    show (h :< b) = show h ++ ":<" ++ show b

instance (BaseClass a) => Element (HatBase a) where
    wiledcard = HatNot :<wiledcard

    {-# INLINE equal #-}
    equal (h1:<b1) (h2:<b2) = h1 .== h2 && b1 .== b2

    keepWiledcard (h1:<b1) (h2:<b2)
        = (keepWiledcard h1 h2) :< (keepWiledcard b1 b2)

    -- |
    -- >>> type Test = HatBase CountUnit
    -- >>> compareHatBase (Not:<Amount :: Test) (Not:<(.#) :: Test)
    -- EQ
    compareElement (h1:<b1) (h2:<b2)
        = case compareElement b1 b2 of
            EQ -> compareElement h1 h2
            x  -> x

instance (BaseClass a) => BaseClass (HatBase a) where

instance (BaseClass a) => HatBaseClass (HatBase a) where
    hat  = _hat

    {-# INLINE toHat #-}
    toHat (h:<b) = Hat:<b
    {-# INLINE toNot #-}
    toNot (h:<b) = Not:<b

    {-# INLINE revHat #-}
    revHat (Hat :< b) = Not :< b
    revHat (Not :< b) = Hat :< b

    {-# INLINE isHat #-}
    isHat  (Hat :< b) = True
    isHat  (Not :< b) = False

    {-# INLINE isNot #-}
    isNot  = not . isHat

------------------------------------------------------------
-- * Define ExBase
------------------------------------------------------------
data Side   = Credit -- 貸方
            | Debit  -- 借方
            | Side -- wiledecard
            deriving (Ord, Show, Eq)

switchSide :: Side -> Side
switchSide Credit = Debit
switchSide Debit  = Credit

data FixedCurrent   = Fixed
                    | Current
                    | Other
                    deriving (Show, Eq)

-- | BaseClass ⊃ HatBaseClass ⊃ ExBaseClass
class (HatBaseClass a) => ExBaseClass a where
    getAccountTitle :: a -> AccountTitles

    setAccountTitle :: a -> AccountTitles -> a

    {-# INLINE (.~) #-}
    (.~) :: a -> AccountTitles -> a
    (.~) = setAccountTitle

    {-# INLINE whatDiv #-}
    whatDiv     :: a -> AccountDivision
    whatDiv b
        | isWiledcard (getAccountTitle b)        = error
                                                 $ "this is wiledcard"
                                                 ++ show (getAccountTitle b)

        | getAccountTitle b == CapitalStock      = Equity
        | getAccountTitle b == RetainedEarnings  = Equity

        | L.elem (getAccountTitle b)
            [ LongTermLoansPayable
            , ShortTermLoansPayable
            , LoansPayable
            , ReserveForDepreciation
            , DepositPayable
            , LongTermNationalBondsPayable
            , ShortTermNationalBondsPayable
            , ReserveDepositPayable
            , CentralBankNotePayable]         = Liability

        | L.elem (getAccountTitle b)
            [ Depreciation
            , CostOfGoodsSold
            , BusinessTrip
            , Commutation
            , UtilitiesExpense
            , RentExpense
            , AdvertisingExpense
            , DeliveryExpenses
            , SuppliesExpenses
            , MiscellaneousExpenses
            , WageExpenditure
            , InterestExpense
            , TaxesExpense
            , ConsumptionExpenditure
            , SubsidyExpense
            , CentralBankPaymentExpense
            , Purchases
            , NetIncome]                      = Cost

        | L.elem (getAccountTitle b)
            [ ValueAdded
            , SubsidyIncome
            , NationalBondInterestEarned
            , DepositInterestEarned
            , GrossProfit
            , OrdinaryProfit
            , InterestEarned
            , ReceiptFee
            , RentalIncome
            , WageEarned
            , TaxesRevenue
            , CentralBankPaymentIncome
            , Sales
            , NetLoss]                        = Revenue
        | otherwise                           = Assets

    {-# INLINE whatPIMO #-}
    whatPIMO    :: a -> PIMO
    whatPIMO x
        | whatDiv x == Assets       = PS
        | whatDiv x == Equity       = MS
        | whatDiv x == Liability    = MS
        | whatDiv x == Cost         = OUT
        | whatDiv x == Revenue      = IN

    {-# INLINE whichSide #-}
    whichSide   :: a -> Side
    whichSide x
        | hat x == Not  = f $ whatDiv x
        | otherwise     = switchSide $ f $ whatDiv x
        where
            {-# INLINE f #-}
            f Assets    = Credit
            f Cost      = Credit
            f Liability = Debit
            f Equity    = Debit
            f Revenue   = Debit

    -- credit :: [a] -- ^ Elem に Text や Int などがある場合は projCredit を使う
    -- credit = L.filter (\x -> whichSide x == Credit) [toEnum 0 ..]

    -- debit :: [a] -- ^ Elem に Text や Int などがある場合は projDebit を使う
    -- debit = L.filter (\x -> whichSide x == Debit) [toEnum 0 ..]

    -- | 流動/固定の区別
    -- 要チェック

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
        f StockInvestment                = Other  -- 注意
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
        f CostOfGoodsSold                = Other
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


class AccountBase a where
    (<=>) :: a -> a -> Bool

data AccountDivision = Assets       -- ^ 資産
                     | Equity       -- ^ 資本
                     | Liability    -- ^ 負債
                     | Cost         -- ^ 費用
                     | Revenue      -- ^ 収益
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

data PIMO   = PS
            | IN
            | MS
            | OUT
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
-- * シンプルな基底 増やしたければ増やせる
-- 同じ呼び出し関数を使うためにタプルにしている.
-- DuplicateRecordFields 拡張 よりも制限が少なく 見た目が良いためにこちらを選択
------------------------------------------------------------------

-- ** 要素数 1
-- *** 勘定科目のみ (交換代数基底)
instance BaseClass AccountTitles where

instance ExBaseClass (HatBase AccountTitles) where
    getAccountTitle (h :< a)   = a
    setAccountTitle (h :< a) b = h :< b

-- ***  名前のみ(冗長代数基底)
instance BaseClass Name where

-- *** CountUnitのみ(冗長代数基底)
instance BaseClass CountUnit where

-- *** Dayのみ(冗長代数基底)
instance BaseClass Day where

-- *** TimeOfDayのみ(冗長代数基底)
instance BaseClass TimeOfDay where

-- ***


-- ** 要素数2

-- | 基礎的なBaseClass 要素数 2

instance ExBaseClass (HatBase (AccountTitles, Day)) where
    getAccountTitle (h:< (a, d))   = a
    setAccountTitle (h:< (a, d)) b = h:< (b, d)

instance ExBaseClass (HatBase (AccountTitles, Name)) where
    getAccountTitle (h:< (a, n))   = a
    setAccountTitle (h:< (a, n)) b = h:< (b, n)

instance ExBaseClass (HatBase (CountUnit, AccountTitles)) where
    getAccountTitle (h:< (u, a))   = a
    setAccountTitle (h:< (u, a)) b = h:< (u, b)

-- ** 要素数 3
-- | 基礎的なBaseClass 要素数 3
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit)) where
    getAccountTitle (h:< (a, n, c))   = a
    setAccountTitle (h:< (a, n, c)) b = h:< (b, n, c)

-- ** 要素数 4
-- | 基礎的なBaseClass 要素数 4
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject)) where
    getAccountTitle (h:< (a, n, c, s))   = a
    setAccountTitle (h:< (a, n, c, s)) b = h:< (b, n, c, s)

-- ** 要素数 5
-- | 基礎的なBaseClass 要素数 5
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject,  Day)) where
    getAccountTitle (h:< (a, n, c, s, d))   = a
    setAccountTitle (h:< (a, n, c, s, d)) b = h:< (b, n, c, s, d)


-- ** 要素数 5
-- | 基礎的なBaseClass 要素数 6
instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject, Day, TimeOfDay)) where
    getAccountTitle (h:< (a, n, c, s, d, t))   = a
    setAccountTitle (h:< (a, n, c, s, d, t)) b = h:< (b, n, c, s, d, t)


