{-# LANGUAGE  MultiParamTypeClasses
            , InstanceSigs
            , TypeSynonymInstances
            , DeriveDataTypeable
            , OverloadedStrings
            , FlexibleInstances
            , FlexibleContexts
            , TypeOperators
            , BangPatterns
            , InstanceSigs
            , TypeFamilies
            , RankNTypes
            , GADTs
            , StrictData
            , Strict                     #-}


{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi15@cs.dis.titech.ac.jp

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}


module ExchangeAlgebra where

import              Debug.Trace
import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.List           as L (foldr1, map, length, elem,sort,foldl1,filter, or, and, sum)
import              Prelude             hiding (map, head, filter,tail, traverse, mapM)
import qualified    Data.Time           as Time
import              Data.Time
import qualified    Data.Map.Strict     as Map
import qualified    Data.Map.Strict     as Map
import qualified    Data.Maybe          as Maybe
import qualified    Number.NonNegative  as NN  -- 非負の実数
import              Numeric.NonNegative.Class (C)
import              Data.Bifunctor
import              Data.Biapplicative


------------------------------------------------------------------
-- * 丸め込み判定
------------------------------------------------------------------
class (Eq a, Ord a) => Nearly a where
    isNearly     :: a -> a -> a -> Bool


instance Nearly Int where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Integer where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Float where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly Double where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

instance Nearly NN.Double where
    {-# INLINE isNearly #-}
    isNearly = isNearlyNum

{-# INLINE isNearlyNum #-}
isNearlyNum :: (Num a, Ord a) => a -> a -> a -> Bool
isNearlyNum x y t
    | x == y  = True
    | x >  y  = abs (x - y) <= abs t
    | x <  y  = abs (y - x) <= abs t




------------------------------------------------------------------
-- * Element 基底の要素
------------------------------------------------------------------

-- | Element Class 基底の要素になるためにはこれのインスタンスになる必要がある
class (Eq a, Ord a, Show a) => Element a where

    wiledcard       ::  a        -- ^ 検索等に用いるワイルドカード

    isWiledcard     :: a -> Bool
    {-# INLINE isWiledcard #-}
    isWiledcard a | wiledcard == a = True
                  | otherwise      = False

    equal :: a -> a -> Bool
    {-# INLINE equal #-}
    equal a b | isWiledcard a = True
              | isWiledcard b = True
              | otherwise     = a == b

    -- | wiledcard を等しいとみなす ==
    (.==) :: a -> a -> Bool
    {-# INLINE (.==)  #-}
    (.==) a b | a == b   || (equal a b) = True
              | otherwise               = False

    -- | wiledcard を等しいとみなす /=
    (./=) :: a -> a -> Bool
    {-# INLINE (./=) #-}
    (./=) a b = not (a .== b)

(.#) :: Element a => a
(.#) = wiledcard


infix 4 .==
infix 4 ./=
------------------------------------------------------------------
-- * Elm
------------------------------------------------------------------

-- ** Account Titles

-- | The current version 0.1.0.0 will be completely changed shortly, especially this section.
data  AccountTitles = Cash                            -- ^ 資産 現金
                    | Deposits                        -- ^ 資産 預金
                    | NationalBonds                   -- ^ 資産 国債
                    | Products                        -- ^ 資産 在庫
                    | Machinery                       -- ^ 資産 機械設備
                    | Building                        -- ^ 資産 不動産
                    | StockInvestment                 -- ^ 資産 株式投資
                    | EquipmentInvestment             -- ^ 資産 設備投資
                    | LoansReceivable                 -- ^ 資産 貸付金
                    | ReserveDepositReceivable        -- ^ 資産 預金準備金
                    | Gold                            -- ^ 資産 金
                    | GovernmentService               -- ^ 資産 政府支出
                    | CapitalStock                    -- ^ 資本 資本金
                    | RetainedEarnings                -- ^ 資本 留保所得
                    | LoansPayable                    -- ^ 負債 借入金
                    | ReserveForDepreciation          -- ^ 負債 償却準備金
                    | DepositPayable                  -- ^ 負債 預り金
                    | NationalBondsPayable            -- ^ 負債 国債 借入金
                    | ReserveDepositPayable           -- ^ 負債 未払金
                    | CentralBankNotePayable          -- ^ 負債 中央銀行手形
                    | Depreciation                    -- ^ 費用
                    | WageExpenditure                 -- ^ 費用
                    | InterestExpense                 -- ^ 費用
                    | TaxesExpense                    -- ^ 費用
                    | ConsumptionExpenditure          -- ^ 費用
                    | SubsidyExpense                  -- ^ 費用
                    | CentralBankPaymentExpense       -- ^ 費用
                    | ValueAdded                      -- ^ 収益
                    | SubsidyIncome                   -- ^ 収益
                    | NationalBondInterestEarned      -- ^ 収益
                    | DepositInterestEarned           -- ^ 収益
                    | GrossProfit                     -- ^ 収益
                    | OrdinaryProfit                  -- ^ 収益
                    | InterestEarned                  -- ^ 収益
                    | WageEarned                      -- ^ 収益
                    | TaxesRevenue                    -- ^ 収益
                    | CentralBankPaymentIncome        -- ^ 収益
                    | AccountTitle                    -- ^ ワイルドカード
                    deriving (Show, Eq, Ord, Enum)

instance Element AccountTitles where
    wiledcard = AccountTitle


{- |

勘定科目の全体と
必要となる処理に関して,体系的に包括された参考文献
それをコピーすれば必要十分な文献

-}

-- | Name :: 品目の名前
type Name = Text

-- | 勘定科目の主体
type Subject = Text
instance Element Text where
    wiledcard   = T.empty


-- | 通貨単位 又は 物量
data CountUnit  = Yen
                | Dollar
                | Euro
                | CNY
                | Amount
                | CountUnit
                deriving (Ord, Show, Eq,Enum)

instance Element CountUnit where
    wiledcard = CountUnit

instance Element TimeOfDay where
    wiledcard = Time.midnight

instance Element Day where
    wiledcard =  ModifiedJulianDay 0

instance (Element a ,Element b)
    => Element (a, b) where
    wiledcard = (wiledcard, wiledcard)
    equal (a1, a2) (b1, b2)
        =  (a1 .== b1)
        && (a2 .== b2)

instance (Element a, Element b, Element c)
    => Element (a, b, c) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard)

    equal (a1, a2, a3) (b1, b2, b3)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)

instance (Element a, Element b, Element c, Element d)
    => Element (a, b, c, d) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    equal (a1, a2, a3, a4) (b1, b2, b3, b4)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)


instance (Element a, Element b, Element c, Element d, Element e)
    => Element (a, b, c, d, e) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    equal (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)

instance (Element a, Element b, Element c, Element d, Element e, Element f)
    => Element (a, b, c, d, e, f) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    equal (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)

instance (Element a, Element b, Element c, Element d, Element e, Element f, Element d)
    => Element (a, b, c, d, e, f, d) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    equal (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)
        && (a7 .== b7)


------------------------------------------------------------------
-- * Base 基底の条件
------------------------------------------------------------------

-- ** Base
------------------------------------------------------------------
{- | 基底の定義
    これを継承すれば取り敢えず基底になれる
-}

class (Element a) =>  BaseClass a where

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
    hat     :: a    -> Hat
    revHat  :: a    -> a
    isHat   :: a    -> Bool
    isNot   :: a    -> Bool

-- | Hat の定義
data Hat = Hat | Not | HatNot deriving (Enum, Show)

instance Eq Hat where
    (==) Hat    Hat     = True
    (==) Not    Not     = True
    (==) Hat    Not     = False
    (==) Not    Hat     = False
    (==) Hat    HatNot  = True
    (==) HatNot Hat     = True
    (==) Not    HatNot  = True
    (==) HatNot Not     = True
    (/=) x y = not (x == y)

instance Ord Hat where
    compare Hat Hat     = EQ
    compare Not Not     = EQ
    compare HatNot _    = EQ
    compare _ HatNot    = EQ
    compare Hat Not     = LT
    compare Not Hat     = GT


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


instance Element Hat where
    wiledcard = HatNot
    equal Hat Hat = True
    equal Hat Not = False
    equal Not Hat = False
    equal Not Not = True
    equal _   _   = True


data HatBase a where
     (:<)  :: (BaseClass a) => {_hat :: Hat,  _base :: a } -> HatBase a

instance Eq (HatBase a) where
    (==) (h1 :< b1) (h2 :< b2) = h1 .== h2 && b1 .== b2
    (/=) x y = not (x == y)

instance Ord (HatBase a) where
    compare (h :< b) (h' :< b')
        | b >  b'  = GT
        | b .== b' = compare h h'
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

instance Element (HatBase a) where
    wiledcard = undefined
    equal (h1:<b1) (h2:<b2) = h1 .== h2 && b1 .== b2

instance BaseClass (HatBase a) where

instance HatBaseClass (HatBase a) where
    hat  = _hat

    revHat (Hat :< b) = Not :< b
    revHat (Not :< b) = Hat :< b

    isHat  (Hat :< b) = True
    isHat  (Not :< b) = False

    isNot  = not . isHat


------------------------------------------------------------
-- * Define ExBase
------------------------------------------------------------
data Side = Credit | Debit deriving (Ord, Show, Eq)

switchSide :: Side -> Side
switchSide Credit = Debit
switchSide Debit  = Credit

-- | BaseClass ⊃ HatBaseClass ⊃ ExBaseClass
class (HatBaseClass a) => ExBaseClass a where
    getAccountTitle :: a -> AccountTitles
    setAccountTitle :: a -> AccountTitles -> a

    whatDiv     :: a -> AccountDivision
    whatDiv b
        | isWiledcard (getAccountTitle b)        = error $ "this is wiledcard" ++ show (getAccountTitle b)

        | getAccountTitle b == CapitalStock      = Equity
        | getAccountTitle b == RetainedEarnings  = Equity

        | L.elem (getAccountTitle b)
            [ LoansPayable
            , ReserveForDepreciation
            , DepositPayable
            , NationalBondsPayable
            , ReserveDepositPayable
            , CentralBankNotePayable]         = Liability

        | L.elem (getAccountTitle b)
            [ Depreciation
            , WageExpenditure
            , InterestExpense
            , TaxesExpense
            , ConsumptionExpenditure
            , SubsidyExpense
            , CentralBankPaymentExpense]      = Cost

        | L.elem (getAccountTitle b)
            [ ValueAdded
            , SubsidyIncome
            , NationalBondInterestEarned
            , DepositInterestEarned
            , GrossProfit
            , OrdinaryProfit
            , InterestEarned
            , WageEarned
            , TaxesRevenue
            , CentralBankPaymentIncome]       = Revenue
        | otherwise                           = Assets


    whatPIMO    :: a -> PIMO
    whatPIMO x
        | whatDiv x == Assets       = PS
        | whatDiv x == Equity       = MS
        | whatDiv x == Liability    = MS
        | whatDiv x == Cost         = OUT
        | whatDiv x == Revenue      = IN

    whichSide   :: a -> Side
    whichSide x
        | hat x == Hat  = f $ whatDiv x
        | otherwise        = switchSide $ f $ whatDiv x
        where
            f Assets    = Credit
            f Cost      = Credit
            f Liability = Debit
            f Equity    = Debit
            f Revenue   = Debit

    -- credit :: [a] -- ^ Elem に Text や Int などがある場合は projCredit を使う
    -- credit = L.filter (\x -> whichSide x == Credit) [toEnum 0 ..]

    -- debit :: [a] -- ^ Elem に Text や Int などがある場合は projDebit を使う
    -- debit = L.filter (\x -> whichSide x == Debit) [toEnum 0 ..]


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

data PIMO   = PS | IN | MS | OUT
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


------------------------------------------------------------
-- * Algebra
------------------------------------------------------------
------------------------------------------------------------------
-- ** Definition of Reducduncy (これを継承すれば冗長代数になる)
------------------------------------------------------------------

-- | Reduncdant Class
--
--  Redundant ⊃ Exchange

class (HatVal n, HatBaseClass b, Monoid (a n b)) =>  Redundant a n b where
    -- | hat calculation
    (.^) :: a n b -> a n b

    -- | bar calculation
    (.-) :: a n b -> a n b

    -- | compress same Base algebras keep dividing different Hat Values
    compress :: a n b -> a n b

    -- | + calculation; alias of <> in monoid
    (.+) :: a n b -> a n b -> a n b

    -- | multiplication
    (.*) :: n -> a n b -> a n b

    -- | get value part
    norm :: a n b -> n

    -- | alias of nolm
    (.|) :: a n b -> n
    (.|) = norm

    (<+) :: (Applicative f) => f (a n b) -> f (a n b) -> f (a n b)
    (<+) x y = (.+) <$> x <*> y


infixr 7 .^
infixr 2 .-
infixr 3 .+
infixr 3 <+


------------------------------------------------------------
-- ** Definition of Exchange Algebra
------------------------------------------------------------
class (Redundant a n b ) => Exchange a n b where
    decR :: a n b -> a n b       -- ^ R-L decomposition
    decL :: a n b -> a n b

    decP :: a n b -> a n b       -- ^ P-M decomposition
    decM :: a n b -> a n b

    balance :: a n b -> Bool -- ^ norm Balance


------------------------------------------------------------------
-- * Algebra
------------------------------------------------------------------

class (Show n, Ord n, Eq n, Nearly n, Fractional n,  Num n, C n) => HatVal n where

instance HatVal NN.Double where

-- | 代数元 数値と基底のペア
--
-- Use (.+) instead of (:+) except for pattern match.

data  Alg n b where
    Zero :: Alg n b
    (:@) :: {_val :: n, _hatBase :: b}  -> Alg n b
    (:+) :: (Alg n b) -> (Alg n b) -> Alg n b

(<@) :: (HatVal n, Applicative f, HatBaseClass b) => f n  -> b -> f (Alg n b)
(<@) v b = (:@) <$> v <*> (pure b)

infixr 6 :@
infixr 6 <@
infixr 5 :+

instance (Show n, Show b) => Show (Alg n b) where
    show Zero               = "0"
    show (v :@ b)           = (show v)          ++ ":@"    ++ show b
    show (x:@b :+ y:@c)     = (show (x:@b))     ++ " .+ "  ++ show (y:@c)
    -- 左結合になっている場合のみ表記変更
    show ((w :+ z) :+ x:@b) = (show (w :+ z))   ++ " |.+ " ++ show (x:@b)
    show (x :+ y)           = (show x)          ++ " .+ "  ++ show y

instance (HatVal n, HatBaseClass b) =>  Eq (Alg n b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False

    (==) (v :@ b) (v' :@ b')
        | v == v' && b .== b'    = True
        | otherwise              = False

    -- 交換法則
    (==) (x :+ y) (w :+ z)       = (x == w && y == z)  || (x == z && y == w)
    (/=) x y                     = not (x == y)

instance (HatVal n, HatBaseClass b) =>  Ord (Alg n b) where
    compare Zero Zero = EQ
    compare Zero _    = LT
    compare _    Zero = GT
    compare (v :@ b) (v' :@ b')
        | b .== b'   = compare v v'
        | otherwise  = compare b b'
    -- :+ に関しては定義しない

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


instance  (HatVal n, HatBaseClass b) => Semigroup (Alg n b) where
    {-# INLINE (<>) #-}

    x <> y  = foldl1 (:+)
            $ (flip L.filter)
            (toList x ++ toList y)
            $ \z -> z /= Zero && (_val z) /= 0
    {-
    (v:@b) <> (w:@c) = case (v == 0 , w == 0) of
                            (True, True)   -> Zero
                            (True, False)  -> (w:@c)
                            (False, True)  -> (v:@b)
                            (False, False) -> (v:@b) :+ (w:@c)

    -- | 単位元の演算
    Zero   <>  Zero   = Zero
    Zero   <> (v:@b)  = case v == 0 of
                            True  -> Zero
                            False -> (v:@b)

    (v:@b) <> Zero    = case v == 0 of
                            True  -> Zero
                            False -> (v:@b)

    (x:+y) <> Zero    = x <> y
    Zero   <> (z:+w)  = z <> w

    -- | 結合法則(というか右結合に変換)
    (v:@b) <> (z:+w) = case v == 0 of
                        True  -> (z <> w)
                        False -> (v:@b) :+ (z <> w)

    (x:+y) <> (w:@c) = case w == 0 of
                        True  -> (x <> y)
                        False -> (w:@c) :+ (x <> y)

    (x:+y) <> (z:+w) = x <> (y <> (z <> w))
    -}

instance (HatVal n, HatBaseClass b) => Monoid (Alg n b) where
    -- 単位元
    mempty = Zero
    mappend = (<>)
    mconcat = foldr mappend mempty

instance Bifunctor Alg where
    bimap _ _ Zero     = Zero
    bimap f g (v:@b)   = (f v) :@ (g b)
    bimap f g (x:+xs)  = (bimap f g x) :+ (bimap f g xs)

instance (HatVal n, HatBaseClass b) => Redundant Alg n b where
    (.^) Zero               = Zero
    (.^) (v :@ b)           = v :@ (revHat b)
    (.^) (x :+ y)           = (.^) x .+ (.^) y

    (.+) = mappend

    x  .*  Zero    = Zero
    0  .*  x       = Zero
    x  .*  (v:@b)  = case v == 0 of
                        True  -> Zero
                        False -> (x * v):@b

    a  .*  (x:+y)  = (a.* x) .+ (a .* y)

    norm Zero       = 0
    norm (v :@ b)   = v
    norm xs         = L.sum $ vals xs

    {-# INLINE (.-) #-}
    (.-) Zero       = Zero
    (.-) (v :@ b)   | v == 0    = Zero
                    | otherwise = v :@ b

    (.-) xs         = f Zero $ L.sort $ toList xs
        where
        {-# INLINE f #-}
        f :: (HatVal n, HatBaseClass b) => Alg n b -> [Alg n b] -> Alg n b
        f x []           = x
        f x [y]          = x >< y
        f Zero   (y:ys)  = f y ys
        f (v:@b) (y:ys)  = case ((v:@b) >< y) of
                                        Zero             -> f Zero   ys
                                        (w:@c)           -> f (w:@c) ys
                                        ((w:@c):+(x:@d)) -> (w:@c) .+ (f (x:@d) ys)

        {-# INLINE (><) #-}
        (><) ::  (HatVal n, HatBaseClass b) => Alg n b -> Alg n b -> Alg n b
        Zero     >< Zero   = Zero
        Zero     >< (v:@b) = case v == 0 of True  -> Zero
                                            False -> (v:@b)
        (v:@b)   >< Zero   = case v == 0 of True  -> Zero
                                            False -> (v:@b)

        (v:@b) >< (w:@c)    | (b /= c)  &&  ((revHat b) /= c) = case (v == 0 , w == 0) of
                                            (True,  True)   -> Zero
                                            (True,  False)  -> (w:@c)
                                            (False, True)   -> (v:@b)
                                            (False, False)  -> (v:@b) :+ (w:@c)
                            | otherwise =  let h = hat b
                                        in let n = hat c
                                        in case (h, n) of
                                            (Hat, Hat) -> (v + w) :@b
                                            (Not, Not) -> (v + w) :@b
                                            (Not, Hat)  | v == w             -> Zero
                                                        | isNearly v w 1e-10 -> Zero -- 丸め込み
                                                        | v >  w             -> (v - w):@b
                                                        | v <  w             -> (w - v):@c
                                            (Hat, Not)  | v == w             -> Zero
                                                        | isNearly v w 1e-10 -> Zero -- 丸め込み
                                                        | v >  w             -> (v - w):@b
                                                        | v <  w             -> (w - v):@c

    {-# INLINE compress #-}
    compress Zero       = Zero
    compress (v :@ b)   | v == 0    = Zero
                        | otherwise = v :@ b

    compress xs         = g Zero $ L.sort $ toList xs
        where
        {-# INLINE g #-}
        g :: (HatVal n, HatBaseClass b) => Alg n b -> [Alg n b] -> Alg n b
        g x []           = x
        g x [y]          = x >><< y
        g Zero   (y:ys)  = g y ys
        g (v:@b) (y:ys)  = case ((v:@b) >><< y) of
                                        Zero             -> g Zero   ys
                                        (w:@c)           -> g (w:@c) ys
                                        ((w:@c):+(x:@d)) -> (w:@c) .+ (g (x:@d) ys)

        {-# INLINE (>><<) #-}
        (>><<) ::  (HatVal n, HatBaseClass b) => Alg n b -> Alg n b -> Alg n b
        Zero    >><< Zero   = Zero
        Zero    >><< (v:@b) = case v == 0 of True  -> Zero
                                             False -> (v:@b)
        (v:@b)  >><< Zero   = case v == 0 of True  -> Zero
                                             False -> (v:@b)

        (v:@b)  >><< (w:@c) | (b /= c)  &&  ((revHat b) /= c) = case (v == 0 , w == 0) of
                                            (True,  True)   -> Zero
                                            (True,  False)  -> (w:@c)
                                            (False, True)   -> (v:@b)
                                            (False, False)  -> (v:@b) :+ (w:@c)
                            | otherwise =  let h = hat b
                                        in let n = hat c
                                        in case (h, n) of
                                            (Hat, Hat) -> (v + w) :@b
                                            (Not, Not) -> (v + w) :@b
                                            (Not, Hat) -> (w:@c) :+ (v:@b)
                                            (Hat, Not) -> (v:@b) :+ (w:@c)

instance (HatVal n, ExBaseClass a) =>  Exchange Alg n a where
    decR xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Debit) xs
    decL xs = filter (\x -> x /= Zero && (whichSide . _hatBase) x == Credit) xs
    decP xs = filter (\x -> x /= Zero && (isHat . _hatBase ) x) xs
    decM xs = filter (\x -> x /= Zero && (not. isHat. _hatBase) x) xs
    balance xs  | (norm . decR) xs == (norm . decL) xs = True
                | otherwise                            = False



------------------------------------------------------------------
-- * 基本の関数
------------------------------------------------------------------

-- | 全てHatかどうかを判定する
--
-- >>> allHat $ 10:@Hat:<Cash .+ 12:@Hat:<Deposits
-- True
--
-- >>> allHat $ 10:@Hat:<Cash .+ 12:@Not:<Deposits
-- False
--
-- Note: in case Zero, this returns True
-- >>> allNot (Zero :: Alg (HatBase AccountTitles))
-- True
--
-- use (.+) instead of (:+)

allHat :: (HatVal n, HatBaseClass b) =>  Alg n b -> Bool
allHat xs = L.and $ L.map (isHat . _hatBase) $ toList xs

-- | 全てNotかどうかを判定する
--
--
-- >>> allNot $ 10:@Hat:<Cash .+ 12:@Hat:<Deposits
-- False
--
-- >>> allNot $ 10:@Not:<Cash .+ 12:@Not:<Deposits
-- True

allNot ::(HatVal n, HatBaseClass b) =>   Alg n b -> Bool
allNot xs = L.and $ L.map (isNot . _hatBase) $ toList xs

vals :: (HatVal n, HatBaseClass b) =>  Alg n b -> [n]
vals Zero     = [0]
vals (x :@ y) = [x]
vals xs = L.map _val $ toList xs

bases ::(HatVal n, HatBaseClass b) =>  Alg n b -> [Maybe b]
bases Zero = [Nothing]
bases (v :@ b) = [Just b]
bases (x :+ y) = bases x ++ bases y

length :: (HatVal n, HatBaseClass b) => Alg n b -> Int
length = L.length . toList

isZero :: (HatVal n, HatBaseClass b) => Alg n b -> Bool
isZero Zero = True
isZero _    = False

isSingle :: (HatVal n, HatBaseClass b) => Alg n b -> Bool
isSIngle (_ :@ _) = True
isSingle _        = False

isFormula :: (HatVal n, HatBaseClass b) => Alg n b -> Bool
isFormula (x :+ y) = True
isFormula _        = False

fromList ::(HatVal n, HatBaseClass b ) => [Alg n b] -> Alg n b
fromList = mconcat

-- | convert Alg n b to List
--
-- >>> toList $ 10:@Hat:<(Cash) :+ 10:@Hat:<(Deposits) :+ Zero
-- [10.0:@Hat:<Cash,10.0:@Hat:<Deposits]
--
-- you need define type variables to use this for Zero
-- >>> toList Zero :: [Alg (HatBase AccountTitles)]
-- []
{-# INLINE toList #-}
toList :: Alg n b -> [Alg n b]
toList Zero     = []
toList (v :@ b) = [(v :@ b)]
toList (x :+ y) = toList x ++ toList y

head :: Alg n b -> Alg n b
head Zero = Zero
head (v :@ b) = (v :@ b)
head (x :+ y) = head x

-- |
tail :: (HatVal n, HatBaseClass b) => Alg n b -> Alg n b
tail Zero            = Zero
tail (v:@b)          = Zero
tail (Zero :+ y)     = y
tail ((v:@ b) :+ y)  = y
tail (x :+ y)        = (tail x) .+ y

{-# INLINE map #-}
-- | map
map :: (HatVal n, HatBaseClass b) => (Alg n a -> Alg n b) -> Alg n a -> Alg n b
map f  Zero    = f Zero
map f (v :@ b) = f (v :@ b)
map f (x :+ y) = (map f x) .+ map f y

traverse :: (HatVal n, HatBaseClass b, Applicative f)
         => (Alg n a -> f (Alg n b)) -> Alg n a -> f (Alg n b)
traverse f xs = fromList <$>  (sequenceA . fmap f)  (toList xs)

{-# INLINE mapM #-}
mapM :: (HatVal n,HatBaseClass b, Applicative f)
     => (Alg n a -> f (Alg n b)) -> Alg n a -> f (Alg n b)
mapM = traverse

{-# INLINE forM #-}
forM :: (HatVal n, HatBaseClass b, Applicative f)
     => Alg n a -> (Alg n a -> f (Alg n b)) -> f (Alg n b)
forM = flip mapM


{-# INLINE filter #-}
-- | filter
filter :: (HatVal n, HatBaseClass b) => (Alg n b -> Bool) -> Alg n b -> Alg n b
filter f Zero                 = Zero
filter f (v:@b) | f (v:@b)    = v:@b
                | otherwise   = Zero
filter f (x:+y) | f x         = case (filter f y) of
                                    Zero -> x
                                    ys   -> x .+ ys
                | otherwise  = filter f y

{- | projection
[\
Let x = \sum_{e_i \in \Gamma}{a_i \times e_i} , then Project[e_k](x) = a_k e_k is defined as projection operatirs.\\
\forall A \subset \Gannma Project[A](x) is defined as Projecton[A](x) = \sum_{e \in A}{project[e](x)}
\]
-}
{-# INLINE proj #-}
proj :: (HatVal n, HatBaseClass b)  => [b] -> Alg n b -> Alg n b
proj bs  alg = filter (f bs) alg
    where
    f ::(HatVal n, HatBaseClass b)  => [b] -> Alg n b  -> Bool
    f _   Zero       = False
    f []  _          = False
    f [b] (v:@eb)    = b .== eb
    f bs  (v:@eb)    = L.or $ L.map (\x -> eb .== x) bs
    f [b] xs         = error $ "error at proj : you might use (:+) instead of (.+)."
    f bs  xs         = error $ "error at proj : you might use (:+) instead of (.+)."

-- | proj devit algs の代わりに Elem に Text や Int などがある場合は projCredit を使う
projCredit :: (HatVal n, ExBaseClass b) => Alg n b -> Alg n b
projCredit = filter (\x -> (whichSide . _hatBase) x == Credit)

-- | proj debit algs の代わりに Elem に Text や Int などがある場合は projDebit を使う
projDebit :: (HatVal n, ExBaseClass b)  => Alg n b -> Alg n b
projDebit = filter (\x -> (whichSide . _hatBase) x == Credit)


projByAccountTitle :: (HatVal n, ExBaseClass b) => AccountTitles -> Alg n b -> Alg n b
projByAccountTitle at alg = filter (f at) alg
    where
        f :: (ExBaseClass b) => AccountTitles -> Alg n b -> Bool
        f at Zero = False
        f at x    = ((getAccountTitle ._hatBase) x) .== at

{-# INLINE projNorm #-}
projNorm :: (HatVal n, HatBaseClass b) => [b] -> Alg n b -> n
projNorm bs alg  = norm $ (.-) $ proj bs alg

-- | Baseの大小（==Algの大小）でソート

sort :: (HatVal n, HatBaseClass b) => Alg n b -> Alg n b
sort Zero      = Zero
sort (v :@ b)  = (v :@ b)
sort (x :+ y)  = foldl1 (.+) $ L.sort $ toList (x .+ y)


-- | normの大小でソート
normSort :: Alg n b -> Alg n b
normSort = undefined

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

-- ** 要素数2

-- | 基礎的なBaseClass 要素数 2
instance ExBaseClass (HatBase (AccountTitles, Name)) where
    getAccountTitle (h:< (a, n))   = a
    setAccountTitle (h:< (a, n)) b = h:< (b, n)

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

-- * バランス

{- | バランスしていない場合の処理 -}
forceBalance = undefined


-- * 端数処理

{- | 端数処理
割り算と掛け算に利用
基本的には切り上げで処理する
勘定科目の乗除には全てこれを適用
-}

rounding :: NN.Double -> NN.Double
rounding = fromIntegral . ceiling



