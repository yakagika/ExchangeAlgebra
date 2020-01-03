{-# LANGUAGE  MultiParamTypeClasses
            , TypeSynonymInstances
            , DeriveDataTypeable
            , OverloadedStrings
            , FlexibleInstances
            , TypeOperators
            , BangPatterns
            , InstanceSigs
            , TypeFamilies
            , RankNTypes
            , GADTs               #-}


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
import qualified    Data.Text       as T
import              Data.Text       (Text)
import qualified    Data.List       as L (map, length, elem,sort,foldl1,filter)
import              Prelude         hiding (map, head, filter,tail)
import qualified    Data.Time       as Time
import              Data.Time
import qualified    Data.Map.Strict as Map
import qualified    Data.List       as List


------------------------------------------------------------------
-- | Definition of Reducduncy (これを継承すれば冗長代数になる)
------------------------------------------------------------------

infixr 9 .~

class Redundant a where
    (.^) :: a -> a
    (.~) :: a -> a
    norm :: a -> Double -- ^ 値の部分だけを抽出

------------------------------------------------------------
-- | Exchange これを継承すれば交換代数になる
------------------------------------------------------------
class (Redundant a) => Exchange a where
    decR :: a -> a       -- ^ R-L decomposition
    decL :: a -> a

    decP :: a -> a       -- ^ P-M decomposition
    decM :: a -> a

    balance :: a -> Bool -- ^ norm Balance



------------------------------------------------------------------
-- * Elm
------------------------------------------------------------------
{- | 要素の定義
    これを継承すれば取り敢えず基底になれる
-}

class (Eq a, Ord a, Show a) =>  BaseClass a where
    accountTitle    ::  a -> Maybe AccountTitles
    name            ::  a -> Maybe Text             -- ^ ここを特定のクラスのインスタンスにしたい
    unit            ::  a -> Maybe CountUnit
    subject         ::  a -> Maybe Text
    date            ::  a -> Maybe Day
    time            ::  a -> Maybe TimeOfDay

-- ** Account Titles

-- | The current version 0.1.0.0 will be completely changed shortly, especially this section.
data  AccountTitles =    Cash                            -- ^ Bellows are Assets (資産)
                    |    Deposits
                    |    NationalBonds
                    |    Products
                    |    StockInvectment
                    |    EquipmentInvestment
                    |    LoansReceivable
                    |    ReserveDepositReceivable
                    |    Gold
                    |    GovernmentService
                    |    CapitalStock                    -- ^ Bellows are Equity (資本)
                    |    RetainedEarnings
                    |    LoansPayable                    -- ^ Bellows are Liability (負債)
                    |    ReserveForDepreciation
                    |    DepositPayable
                    |    NationalBondsPayable
                    |    ReserveDepositPayable
                    |    CentralBankNotePayable
                    |    Depreciation                    -- ^ Bellows are Cost
                    |    WageExpenditure
                    |    InterestExpense
                    |    TaxesExpense
                    |    ConsumptionExpenditure
                    |    SubsidyExpense
                    |    CentralBankPaymentExpence
                    |    ValueAdded                      -- ^ Bellows are Revenue
                    |    SubsidyIncome
                    |    NationalBondInterestEarned
                    |    DepositInterestEarned
                    |    GrossProfit
                    |    OrdinaryProfit
                    |    InterestEarned
                    |    WageEarned
                    |    TaxesRevenue
                    |    CentralBankPaymentIncome
                    deriving (Show, Eq, Ord, Enum)

{- |

勘定科目の全体と
必要となる処理に関して,体系的に包括された参考文献
それをコピーすれば必要十分な文献

-}


instance BaseClass AccountTitles where
    accountTitle    a  = Just a
    name            _  = Nothing
    unit            _  = Nothing
    subject         _  = Nothing
    time            _  = Nothing
    date            _  = Nothing

-- | Name :: 品目の名前
type Name = Text

-- | 勘定科目の主体
type Subject = Text


data CountUnit = Yen | Amount deriving (Ord, Show, Eq)


{- | 冗長基底の定義
    これを継承すれば基底になれる.

    何個でも要素数は増やせる.-}

class (BaseClass a) =>  HatBaseClass a where
    getHat  :: a    -> Hat
    revHat  :: a    -> a
    isHat   :: a    -> Bool


-- | Hat の定義
data Hat = Hat | Not deriving (Ord, Eq)

instance Show Hat where
    show Hat = "^"
    show Not = ""

data HatBase a where
     (:<)  :: (BaseClass a) => {hat :: Hat,  base :: a } -> HatBase a

instance Eq (HatBase a) where
    (==) (h1 :< b1) (h2 :< b2) = b1 == b2
    (/=) x y = not (x == y)

instance Ord (HatBase a) where
    compare (h :< b) (h' :< b')
        | b >  b' = GT
        | b == b' = EQ
        | b <  b' = LT

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
    show (h :< b) = show h ++ ":<" ++ show b ++ ">"

instance BaseClass (HatBase a) where
    accountTitle (h :< b) = accountTitle b
    name         (h :< b) = name         b
    unit         (h :< b) = unit         b
    date         (h :< b) = date         b
    subject      (h :< b) = subject      b
    time         (h :< b) = time         b

instance HatBaseClass (HatBase a) where
    getHat (h   :< b) = h
    revHat (Hat :< b) = Not :< b
    revHat (Not :< b) = Hat :< b
    isHat  (Hat :< b) = True
    isHat  (Not :< b) = False


------------------------------------------------------------
-- * Define ExBase
------------------------------------------------------------
class (HatBaseClass a) => ExBaseClass a where
    whatDiv     :: a -> Maybe AccountDivision
    whatDiv b
        | accountTitle b == Nothing                = Nothing
        | accountTitle b == Just CapitalStock      = Just Equity
        | accountTitle b == Just RetainedEarnings  = Just Equity

        | L.elem (accountTitle b)
            [ Just LoansPayable
            , Just ReserveForDepreciation
            , Just DepositPayable
            , Just NationalBondsPayable
            , Just ReserveDepositPayable
            , Just CentralBankNotePayable]         = Just Liability

        | L.elem (accountTitle b)
            [ Just Depreciation
            , Just WageExpenditure
            , Just InterestExpense
            , Just TaxesExpense
            , Just ConsumptionExpenditure
            , Just SubsidyExpense
            , Just CentralBankPaymentExpence]      = Just Cost

        | L.elem (accountTitle b)
            [ Just ValueAdded
            , Just SubsidyIncome
            , Just NationalBondInterestEarned
            , Just DepositInterestEarned
            , Just GrossProfit
            , Just OrdinaryProfit
            , Just InterestEarned
            , Just WageEarned
            , Just TaxesRevenue
            , Just CentralBankPaymentIncome]       = Just Revenue
        | otherwise                                = Just Assets

    whatPIMO    :: a -> Maybe PIMO
    whatPIMO x
        | whatDiv x == Nothing           = Nothing
        | whatDiv x == Just Assets       = Just PS
        | whatDiv x == Just Equity       = Just MS
        | whatDiv x == Just Liability    = Just MS
        | whatDiv x == Just Cost         = Just OUT
        | whatDiv x == Just Revenue      = Just IN

    whichSide   :: a -> Maybe Side
    whichSide x
        | getHat x == Hat  = f $ whatDiv x
        | otherwise        = switchSide <$> f (whatDiv x)
        where
            f Nothing          = Nothing
            f (Just Assets   ) = Just Credit
            f (Just Cost     ) = Just Credit
            f (Just Liability) = Just Debit
            f (Just Equity   ) = Just Debit
            f (Just Revenue  ) = Just Debit

instance ExBaseClass (HatBase a) where


class AccountBase a where
    (<=>) :: a -> a -> Bool

data AccountDivision = Assets | Equity | Liability | Cost | Revenue
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

data Side = Credit | Debit deriving (Ord, Show, Eq)

switchSide :: Side -> Side
switchSide Credit = Debit
switchSide Debit  = Credit



------------------------------------------------------------
-- * Algebra
------------------------------------------------------------
infixr 7 :@
infixr 5 :+

-- instance (HatBase a) => AlgClass a where
--    isZero  :: a -> Bool
--    getVal  :: a -> Double


{-@ type R = {i:Int | i >= 0 }@-}
-- | 交換代数元 数値と基底のペア
data ExAlg b where
    Zero :: (ExBaseClass b) => ExAlg b
    {-@ (:@) ::  (BaseClass b) => {val :: !R, hatBase :: !b}  -> ExAlg b @-}
    (:@) :: (ExBaseClass b) => {val :: !Double, hatBase :: !b}  -> ExAlg b
    (:+) :: (ExBaseClass b) => !(ExAlg b) -> !(ExAlg b) -> ExAlg b


instance (ExBaseClass a) =>  Exchange (ExAlg a) where
    decR xs = fromList $ filter (\x -> x /= Zero && (whichSide . hatBase) x == Just Debit) xs
    decL xs = fromList $ filter (\x -> x /= Zero && (whichSide . hatBase) x == Just Credit) xs
    decP xs = fromList $ filter (\x -> x /= Zero && (isHat . hatBase ) x) xs
    decM xs = fromList $ filter (\x -> x /= Zero && (not. isHat. hatBase) x) xs
    balance xs  | (norm . decR) xs == (norm . decL) xs = True
                | otherwise                            = False

instance (Show b) => Show (ExAlg b) where
    show Zero           = "0"
    show (v :@ b)       = (show v) ++ ":@" ++  (show b)
    show (x :+ y)       = (show x) ++ ".+ " ++ (show y)

instance (ExBaseClass b) => Eq (ExAlg b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False

    (==) (v :@ b) (v' :@ b')
        | v == v' && b == b'    = True
        | otherwise             = False

    (==) x y = f x == f y
        where f = (L.filter (Zero <)) . L.sort . toList

    (/=) x y = not (x == y)


instance (ExBaseClass b, Ord b) => Ord (ExAlg b) where
    compare Zero Zero = EQ
    compare Zero _    = LT
    compare _    Zero = GT
    compare (v :@ b) (v' :@ b')
        | b == b'   = compare v v'
        | otherwise = compare b b'
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


instance (ExBaseClass b) => Semigroup (ExAlg b) where
    (<>) Zero Zero  = Zero
    (<>) Zero !x     = x
    (<>) !x    Zero  = x
    (<>) !(v :@ b) !(v' :@ b') = (v :@ b) :+ (v' :@ b')
    (<>) !x  !y =  f ( x :+ y)
        where f = (foldl1 (:+)) . (L.filter (/= Zero)) . toList

instance (ExBaseClass b) => Monoid (ExAlg b) where
    mempty = Zero
    mappend = (<>)
    mconcat []       = Zero
    mconcat [Zero]   = Zero
    mconcat [v :@ b] = v :@ b
    mconcat (x:y) = x `mappend`  mconcat y

-- | :+ の Evaluationに相当する
infixr 5 .+
(.+) :: (ExBaseClass b) => ExAlg b -> ExAlg b -> ExAlg b
(.+) = mappend

instance (ExBaseClass b) =>  Redundant (ExAlg b) where
    (.^) Zero               = Zero
    (.^) (v :@ b)           = v :@ (revHat b)
    (.^) (x :+ y)           = map (.^) x :+ map (.^) y


    norm Zero       = 0
    norm (v :@ b)   = v
    norm xs         = sum $ vals xs

    (.~) Zero       = Zero
    (.~) (v :@ b)   | v == 0.0  = Zero
                    | otherwise = v :@ b
    (.~) ((v :@ b) :+ (v' :@ b'))
        | b /= b' = ((v :@ b) :+ (v' :@ b'))
        | otherwise
            = let h   = getHat b  in
                let h'  = getHat b' in
                case (h, h') of
                (Hat, Hat) -> (v + v') :@ b
                (Not, Not) -> (v + v') :@ b
                (Not, Hat)  | v == v' -> Zero
                            | v >  v' -> (v - v') :@ b
                            | v <  v' -> (v' - v) :@ b'
                (Hat, Not)  | v == v' -> Zero
                            | v >  v' -> (v - v') :@ b
                            | v <  v' -> (v' - v) :@ b'


    (.~) xs = mconcat $ filter g $ f z
        where
            g :: ExAlg b -> Bool
            g Zero     = False
            g (0 :@ _) = False
            g _        = True

            z = sort xs
            f :: (ExBaseClass b) => ExAlg b  -> ExAlg b
            f Zero       = Zero
            f (v :@ b)   | v == 0.0  = Zero
                         | otherwise = v :@ b
            f ((v :@ b) :+ (v' :@ b'))  = (.~) ((v :@ b) :+ (v' :@ b'))
            f xs    | isZero h1              = f t
                    | hatBase h1 /= hatBase h2     = h1 :+ f t
                    | otherwise = f $ (f (h1 :+ h2)) :+ tail t
                    where
                        t  = tail xs
                        h1 = head xs
                        h2 = head t

------------------------------------------------------------------
-- * 基本の関数
------------------------------------------------------------------
vals :: ExAlg b -> [Double]
vals Zero     = [0]
vals (x :@ y) = [x]
vals xs = L.map val $ toList xs

bases :: ExAlg b -> [Maybe b]
bases Zero = [Nothing]
bases (v :@ b) = [Just b]
bases (x :+ y) = bases x ++ bases y

length :: ExAlg b -> Int
length = L.length . toList

isZero :: ExAlg b -> Bool
isZero Zero = True
isZero _    = False

isSingle :: ExAlg b -> Bool
isSIngle (_ :@ _) = True
isSingle _        = False

isFormula :: ExAlg b -> Bool
isFormula (x :+ y) = True
isFormula _        = False

fromList :: (ExBaseClass b) =>  [ExAlg b] -> ExAlg b
fromList = mconcat

toList :: ExAlg b -> [ExAlg b]
toList Zero     = [Zero]
toList (v :@ b) = [(v :@ b)]
toList (x :+ y) = toList x ++ toList y

head :: ExAlg b -> ExAlg b
head Zero = Zero
head (v :@ b) = (v :@ b)
head (x :+ y) = head x

tail :: ExAlg b -> ExAlg b
tail Zero = Zero
tail (v:@b) = Zero
tail (Zero :+ y) =  y
tail ((v:@ b) :+ y) = y
tail (x :+ y) = (tail x) :+ y

{-# INLINE map #-}
-- | map
map ::   (ExAlg b -> ExAlg b) -> ExAlg b -> ExAlg b
map f  Zero    = f Zero
map f (v :@ b) = f (v :@ b)
map f (x :+ y) = (map f x) :+ map f y

{-# INLINE filter #-}
-- | filter
filter :: (ExAlg b -> Bool) -> ExAlg b -> [ExAlg b]
filter f Zero       | f Zero        = [Zero]
                    | otherwise     = []
filter f (v :@ b)   | f (v :@ b)    = [v :@ b]
                    | otherwise     = []
filter f (x :+ y) = filter f x ++ filter f y

-- | projection

proj :: (ExBaseClass b) => b -> ExAlg b -> ExAlg b
proj b alg = fromList $ filter (\x ->  x /= Zero && hatBase x == b ) alg


projNorm :: (ExBaseClass b) => b -> ExAlg b -> Double
projNorm b alg  = norm $ (.~)
            $ fromList
            $ filter (\x ->  x /= Zero && hatBase x == b ) alg


-- | Baseの大小（==Algの大小）でソート

sort :: (Ord b) => ExAlg b -> ExAlg b
sort Zero      = Zero
sort (v :@ b)  = (v :@ b)
sort (x :+ y)  = foldl1 (:+) $ L.sort $ toList (x :+ y)


-- | normの大小でソート
normSort :: ExAlg b -> ExAlg b
normSort = undefined

------------------------------------------------------------------
-- * シンプルな基底 増やしたければ増やせる
------------------------------------------------------------------

instance BaseClass (AccountTitles, Name) where
    accountTitle    (a, n)  = Just a
    name            (a, n)  = Just n
    unit            _       = Nothing
    subject         (a, n)  = Nothing
    date            _       = Nothing
    time            _       = Nothing

instance BaseClass (AccountTitles, Name, CountUnit) where
    accountTitle    (a, n, u) = Just a
    name            (a, n, u) = Just n
    unit            (a, n, u) = Just u
    subject         (a, n, u) = Nothing
    date            _         = Nothing
    time            _         = Nothing

instance BaseClass (AccountTitles, Name, CountUnit, Subject) where
    accountTitle (a, n, u, s)   = Just a
    name         (a, n, u, s)   = Just n
    unit         (a, n, u, s)   = Just u
    subject      (a, n, u, s)   = Just s
    date         _              = Nothing
    time         _              = Nothing


instance BaseClass (AccountTitles, Name, CountUnit, Subject,  Day) where
    accountTitle    (a, n, u, s, d) = Just a
    name            (a, n, u, s, d) = Just n
    unit            (a, n, u, s, d) = Just u
    subject         (a, n, u, s, d) = Just s
    date            (a, n, u, s, d) = Just d
    time            _               = Nothing

instance BaseClass (AccountTitles, Name, CountUnit, Subject, Day, TimeOfDay) where
    accountTitle    (a, n, u, s, d, t) = Just a
    name            (a, n, u, s, d, t) = Just n
    unit            (a, n, u, s, d, t) = Just u
    subject         (a, n, u, s, d, t) = Just s
    date            (a, n, u, s, d, t) = Just d
    time            (a, n, u, s, d, t) = Just t

------------------------------------------------------------------
-- * 基本計算処理
------------------------------------------------------------------
-- ** 仕分け


-- ** バランス


-- ** 振替


-- | 振替変換テーブル
data TransTable b where
     TransTable         :: (ExBaseClass b) => (Map.Map b {-変換前-} b {-変換後-})
                                           -> TransTable b
     -- ^ 基底ごと変換
     PartialTransTable  :: (ExBaseClass b) => (Map.Map b (Double {- 変換量 -}, b))
                                           -> TransTable b
     -- ^ 部分的に変換
     FunctionTransTable :: (ExBaseClass b) => (Map.Map b ((Double -> Double), b))
                                           -> TransTable b
     -- ^ 数値部分に関数を適用して変換 按分はこれを使う



instance Show (TransTable b) where
    show (TransTable b)         = "TransTable "         ++ show b
    show (PartialTransTable b)  = "PartialTransTable "  ++ show b
    show (FunctionTransTable b) = "FunctionTransTable " ++
                                (List.foldl1 (++)
                                ( List.map (\(before, (f, after)) -> "(" ++ show before ++ ", ( <function:: Double -> Double>, "
                                ++ show after ++ ")")
                                (Map.toList b)))


{-| 振替変換
振替変換は、交換代数元の要素の基底を別の基底に振り替える変換となります。
振替変換では、変換 対象の値は変わりません。例えば、次のような交換代数元 x があり、
x = 6^ < e1 > +2 < e2 > +2 < e3 > +4 < e4 > +5^ < e5 > 変換定義 t を次のように定義した場合、
( from) < e1 > -> (to) < eA >
( from) < e2 > -> (to) < eA >
( from) < e3 > -> (to) < eA >
変換結果 r は、次のようになります。
r = 6^ < e1 > +2 < e2 > +2 < e3 > +4 < e4 > +5^ < e5 >
   +  6 < e 1 > + 6 ^ < e A >
   + 2 ^ < e 2 > + 2 < e A >
   + 2 ^ < e 3 > + 2 < e A >
 = 6 ^ < e 1 > + 2 < e 2 > + 2 < e 3 > + 4 < e 4 > + 5 ^ < e 5 >
   + 6 < e 1 > + 6 ^ < e A > + 2 ^ < e 2 > + 4 < e A > + 2 ^ < e 3 >
-}

transfer :: (ExBaseClass b) => TransTable b -> ExAlg b -> ExAlg b
transfer _  Zero  = Zero
transfer (TransTable ms)         (v :@ before)
    = case Map.lookup before ms of
        Just after -> v :@ after
        Nothing    -> v :@ before

transfer (PartialTransTable ms)  (v :@ before)
    = case Map.lookup before ms of
        Just (d, after) | d < 0      -> error "transfer: minus value"
                        | v - d >= 0 ->  (v - d) :@ before
                                     .+       d  :@ after
                        | otherwise  ->       v  :@ after
        Nothing         ->                    v  :@ before

transfer (FunctionTransTable ms) (v :@ before) -- 負に変換される場合はrevHat
    = case Map.lookup before ms of
        Just (f, after) | (f v) <  0 -> error "transfer: minus value"
                        | (f v) >= 0 -> (v - (f v)) :@ before
                                     .+      (f v)  :@ after
                        | otherwise  ->          v  :@ after
        Nothing                      ->          v  :@ before

transfer tm xs                                 = map (transfer tm) xs



-- *** 決算振替仕訳

-- *** 経過勘定の振替









