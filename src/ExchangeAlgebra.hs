{-# LANGUAGE  MultiParamTypeClasses
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
import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.List           as L (map, length, elem,sort,foldl1,filter)
import              Prelude             hiding (map, head, filter,tail)
import qualified    Data.Time           as Time
import              Data.Time
import qualified    Data.Map.Strict     as Map
import qualified    Data.Map.Strict     as Map
import qualified    Data.Maybe          as Maybe
import qualified    Control.Lens        as Lens
import              Number.NonNegative  (Double, fromNumber, toNumber) -- 非負の実数

------------------------------------------------------------------
-- * Elements 基底の要素\
------------------------------------------------------------------

-- | Element Class 基底の要素になるためにはこれのインスタンスになる必要がある
class (Eq a, Ord a, Show a) => Element a where


------------------------------------------------------------------
-- * Elm
------------------------------------------------------------------

-- ** Account Titles

-- | The current version 0.1.0.0 will be completely changed shortly, especially this section.
data  AccountTitles =   Cash                            -- ^ 資産 現金
                    |   Deposits                        -- ^ 資産 預金
                    |   NationalBonds                   -- ^ 資産 国債
                    |   Products                        -- ^ 資産 在庫
                    |   Machinery                       -- ^ 資産 機械設備
                    |   Building                        -- ^ 資産 不動産
                    |   StockInvestment                 -- ^ 資産 株式投資
                    |   EquipmentInvestment             -- ^ 資産 設備投資
                    |   LoansReceivable                 -- ^ 資産 貸付金
                    |   ReserveDepositReceivable        -- ^ 資産 預金準備金
                    |   Gold                            -- ^ 資産 金
                    |   GovernmentService               -- ^ 資産 政府支出
                    |   CapitalStock                    -- ^ 資本 資本金
                    |   RetainedEarnings                -- ^ 資本 留保所得
                    |   LoansPayable                    -- ^ 負債 借入金
                    |   ReserveForDepreciation          -- ^ 負債 償却準備金
                    |   DepositPayable                  -- ^ 負債 預り金
                    |   NationalBondsPayable            -- ^ 負債 国債 借入金
                    |   ReserveDepositPayable           -- ^ 負債 未払金
                    |   CentralBankNotePayable          -- ^ 負債 中央銀行手形
                    |   Depreciation                    -- ^ 費用
                    |   WageExpenditure                 -- ^ 費用
                    |   InterestExpense                 -- ^ 費用
                    |   TaxesExpense                    -- ^ 費用
                    |   ConsumptionExpenditure          -- ^ 費用
                    |   SubsidyExpense                  -- ^ 費用
                    |   CentralBankPaymentExpence       -- ^ 費用
                    |   ValueAdded                      -- ^ 収益
                    |   SubsidyIncome                   -- ^ 収益
                    |   NationalBondInterestEarned      -- ^ 収益
                    |   DepositInterestEarned           -- ^ 収益
                    |   GrossProfit                     -- ^ 収益
                    |   OrdinaryProfit                  -- ^ 収益
                    |   InterestEarned                  -- ^ 収益
                    |   WageEarned                      -- ^ 収益
                    |   TaxesRevenue                    -- ^ 収益
                    |   CentralBankPaymentIncome        -- ^ 収益
                    deriving (Show, Eq, Ord, Enum)
instance Element AccountTitles

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

-- | 通貨単位 又は 物量
data CountUnit = Yen | Dollar | Euro | CNY | Amount deriving (Ord, Show, Eq)
instance Element CountUnit where


instance Element TimeOfDay where

instance Element Day where

------------------------------------------------------------------
-- * Base 基底の条件
------------------------------------------------------------------

-- ** Base
------------------------------------------------------------------
data AutoGetter = AutoGetter { _maybeGetAccountTitle :: (Maybe AccountTitles)
                             , _maybeGetName         :: (Maybe Name)
                             , _maybeGetCountUnit    :: (Maybe CountUnit)
                             , _maybeGetSubject      :: (Maybe Subject)
                             , _maybeGetDay          :: (Maybe Day)
                             , _maybeGetTime         :: (Maybe TimeOfDay) } deriving Show

initAutoGetter = AutoGetter Nothing Nothing Nothing Nothing Nothing Nothing

-- | Setter 自動生成用 自動生成になっていない Lensを利用したら可能にも思うが,多相が難しい
data AutoSetter a = AutoSetter  { _maybeSetAccountTitle :: (a -> AccountTitles -> Maybe a)
                                , _maybeSetName         :: (a -> Name          -> Maybe a)
                                , _maybeSetCountunit    :: (a -> CountUnit     -> Maybe a)
                                , _maybeSetSubject      :: (a -> Subject       -> Maybe a)
                                , _maybeSetDay          :: (a -> Day           -> Maybe a)
                                , _maybeSetTime         :: (a -> TimeOfDay     -> Maybe a)}

initAutoSetter = AutoSetter (\x y -> Nothing)
                            (\x y -> Nothing)
                            (\x y -> Nothing)
                            (\x y -> Nothing)
                            (\x y -> Nothing)
                            (\x y -> Nothing)

{- | 要素の定義
    これを継承すれば取り敢えず基底になれる
-}

class (Eq a, Ord a, Show a) =>  BaseClass a where
    autoGetter      :: a -> AutoGetter                         -- ^ Getterだけは自動化可能 ただし,独自の要素を追加した場合は自分で定義する必要がある.
    autoSetter      :: a -> AutoSetter a

    -- getter
    maybeGetAccountTitle  :: a -> Maybe AccountTitles
    maybeGetName          :: a -> Maybe Text
    maybeGetCountUnit     :: a -> Maybe CountUnit
    maybeGetSubject       :: a -> Maybe Text
    maybeGetDay           :: a -> Maybe Day
    maybeGetTime          :: a -> Maybe TimeOfDay

    maybeGetAccountTitle  a = _maybeGetAccountTitle  (autoGetter a)
    maybeGetName          a = _maybeGetName          (autoGetter a)
    maybeGetCountUnit     a = _maybeGetCountUnit     (autoGetter a)
    maybeGetSubject       a = _maybeGetSubject       (autoGetter a)
    maybeGetDay           a = _maybeGetDay           (autoGetter a)
    maybeGetTime          a = _maybeGetTime          (autoGetter a)

    -- setter 自動化はできていないので自分で定義する必要がある
    maybeSetAccountTitle  :: a -> AccountTitles -> Maybe a
    maybeSetName          :: a -> Text          -> Maybe a
    maybeSetCountunit     :: a -> CountUnit     -> Maybe a
    maybeSetSubject       :: a -> Text          -> Maybe a
    maybeSetDay           :: a -> Day           -> Maybe a
    maybeSetTime          :: a -> TimeOfDay     -> Maybe a

    maybeSetAccountTitle a x = (_maybeSetAccountTitle  (autoSetter a)) a x
    maybeSetName         a x = (_maybeSetName          (autoSetter a)) a x
    maybeSetCountunit    a x = (_maybeSetCountunit     (autoSetter a)) a x
    maybeSetSubject      a x = (_maybeSetSubject       (autoSetter a)) a x
    maybeSetDay          a x = (_maybeSetDay           (autoSetter a)) a x
    maybeSetTime         a x = (_maybeSetTime          (autoSetter a)) a x

------------------------------------------------------------------
-- ** HatBase
------------------------------------------------------------------
class (BaseClass a) =>  HatBaseClass a where
    getHat  :: a    -> Hat
    revHat  :: a    -> a
    isHat   :: a    -> Bool

-- | Hat の定義
data Hat = Hat | Not deriving (Ord, Eq)
instance Element Hat where

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

instance HatBaseClass (HatBase a) where
    getHat (h   :< b) = h
    revHat (Hat :< b) = Not :< b
    revHat (Not :< b) = Hat :< b
    isHat  (Hat :< b) = True
    isHat  (Not :< b) = False


------------------------------------------------------------
-- * Define ExBase
------------------------------------------------------------

-- | BaseClass ⊃ HatBaseClass ⊃ ExBaseClass
class (HatBaseClass a) => ExBaseClass a where
    getAccountTitle :: a -> AccountTitles
    setAccountTitle :: a -> AccountTitles -> a

    whatDiv     :: a -> AccountDivision
    whatDiv b
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
            , CentralBankPaymentExpence]      = Cost

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
        | getHat x == Hat  = f $ whatDiv x
        | otherwise        = switchSide $ f $ whatDiv x
        where
            f Assets    = Credit
            f Cost      = Credit
            f Liability = Debit
            f Equity    = Debit
            f Revenue   = Debit

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

data Side = Credit | Debit deriving (Ord, Show, Eq)

switchSide :: Side -> Side
switchSide Credit = Debit
switchSide Debit  = Credit

------------------------------------------------------------
-- * Algebra
------------------------------------------------------------
------------------------------------------------------------------
-- ** Definition of Reducduncy (これを継承すれば冗長代数になる)
------------------------------------------------------------------

-- | Reduncdant Class
--
--  Redundant ⊃ Exchange

class Redundant a where
    (.^) :: a -> a
    (.-) :: a -> a
    (.+) :: a -> a -> a
    norm :: a -> Number.NonNegative.Double  -- ^ 値の部分だけを抽出

infixr 7 .^
infixr 4 .-
infixr 5 .+


------------------------------------------------------------
-- ** Definition of Exchange Algebra
------------------------------------------------------------
class (Redundant a) => Exchange a where
    decR :: a -> a       -- ^ R-L decomposition
    decL :: a -> a

    decP :: a -> a       -- ^ P-M decomposition
    decM :: a -> a

    balance :: a -> Bool -- ^ norm Balance


------------------------------------------------------------------
-- * Algebra
------------------------------------------------------------------

{-@ type R = {i:Int | i >= 0 }@-}
-- | 代数元 数値と基底のペア
data Alg b where
    Zero :: (HatBaseClass b) => Alg b
    {-@ (:@) ::  (BaseClass b) => {val :: !R, hatBase :: !b}  -> Alg b @-}
    (:@) :: (HatBaseClass b) => {val :: Number.NonNegative.Double, hatBase :: !b}  -> Alg b
    (:+) :: (HatBaseClass b) => !(Alg b) -> !(Alg b) -> Alg b

infixr 6 :@
infixr 5 :+

instance (Show b) => Show (Alg b) where
    show Zero           = "0"
    show (v :@ b)       = (show v) ++ ":@" ++  (show b)
    show (x :+ y)       = (show x) ++ ".+ " ++ (show y)

instance (HatBaseClass b) => Eq (Alg b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False

    (==) (v :@ b) (v' :@ b')
        | v == v' && b == b'    = True
        | otherwise             = False

    (==) x y = f x == f y
        where f = (L.filter (Zero <)) . L.sort . toList

    (/=) x y = not (x == y)


instance (HatBaseClass b, Ord b) => Ord (Alg b) where
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


instance (HatBaseClass b) => Semigroup (Alg b) where
    (<>) Zero Zero  = Zero
    (<>) Zero !x     = x
    (<>) !x    Zero  = x
    (<>) !(v :@ b) !(v' :@ b') = (v :@ b) :+ (v' :@ b')
    (<>) !x  !y =  f ( x :+ y)
        where f = filter (/= Zero)

instance (HatBaseClass b) => Monoid (Alg b) where
    mempty = Zero
    mappend = (<>)
    mconcat []       = Zero
    mconcat [Zero]   = Zero
    mconcat [v :@ b] = v :@ b
    mconcat (x:y) = x `mappend`  mconcat y


instance (HatBaseClass b) =>  Redundant (Alg b) where
    (.^) Zero               = Zero
    (.^) (v :@ b)           = v :@ (revHat b)
    (.^) (x :+ y)           = map (.^) x :+ map (.^) y

    (.+) = mappend

    norm Zero       = 0
    norm (v :@ b)   = v
    norm xs         = sum $ vals xs

    (.-) Zero       = Zero
    (.-) (v :@ b)   | v == 0.0  = Zero
                    | otherwise = v :@ b
    (.-) ((v :@ b) :+ (v' :@ b'))
        | b /= b' = ((v :@ b) :+ (v' :@ b'))
        | otherwise
            = let h   = getHat b  in
                let h'  = getHat b' in
                case (h, h') of
                (Hat, Hat) -> (v + v') :@ b
                (Not, Not) -> (v + v') :@ b
                (Not, Hat)  | v == v' -> Zero
                            | v >  v' -> (v - v'):@ b
                            | v <  v' -> (v'- v) :@ b'
                (Hat, Not)  | v == v' -> Zero
                            | v >  v' -> (v - v') :@ b
                            | v <  v' -> (v' - v) :@ b'


    (.-) xs = filter g $ f z
        where
            g :: Alg b -> Bool
            g Zero     = False
            g (0 :@ _) = False
            g _        = True

            z = sort xs
            f :: (HatBaseClass b) => Alg b  -> Alg b
            f Zero       = Zero
            f (v :@ b)   | v == 0.0  = Zero
                         | otherwise = v :@ b
            f ((v :@ b) :+ (v' :@ b'))  = (.-) ((v :@ b) :+ (v' :@ b'))
            f xs    | isZero h1              = f t
                    | hatBase h1 /= hatBase h2     = h1 :+ f t
                    | otherwise = f $ (f (h1 :+ h2)) :+ tail t
                    where
                        t  = tail xs
                        h1 = head xs
                        h2 = head t

instance (ExBaseClass a) =>  Exchange (Alg a) where
    decR xs = filter (\x -> x /= Zero && (whichSide . hatBase) x == Debit) xs
    decL xs = filter (\x -> x /= Zero && (whichSide . hatBase) x == Credit) xs
    decP xs = filter (\x -> x /= Zero && (isHat . hatBase ) x) xs
    decM xs = filter (\x -> x /= Zero && (not. isHat. hatBase) x) xs
    balance xs  | (norm . decR) xs == (norm . decL) xs = True
                | otherwise                            = False



------------------------------------------------------------------
-- * 基本の関数
------------------------------------------------------------------
vals :: Alg b -> [Number.NonNegative.Double]
vals Zero     = [0]
vals (x :@ y) = [x]
vals xs = L.map val $ toList xs

bases :: Alg b -> [Maybe b]
bases Zero = [Nothing]
bases (v :@ b) = [Just b]
bases (x :+ y) = bases x ++ bases y

length :: Alg b -> Int
length = L.length . toList

isZero :: Alg b -> Bool
isZero Zero = True
isZero _    = False

isSingle :: Alg b -> Bool
isSIngle (_ :@ _) = True
isSingle _        = False

isFormula :: Alg b -> Bool
isFormula (x :+ y) = True
isFormula _        = False

fromList :: (HatBaseClass b) =>  [Alg b] -> Alg b
fromList = mconcat

toList :: Alg b -> [Alg b]
toList Zero     = [Zero]
toList (v :@ b) = [(v :@ b)]
toList (x :+ y) = toList x ++ toList y

head :: Alg b -> Alg b
head Zero = Zero
head (v :@ b) = (v :@ b)
head (x :+ y) = head x

tail :: Alg b -> Alg b
tail Zero = Zero
tail (v:@b) = Zero
tail (Zero :+ y) =  y
tail ((v:@ b) :+ y) = y
tail (x :+ y) = (tail x) :+ y

{-# INLINE map #-}
-- | map
map ::   (Alg b -> Alg b) -> Alg b -> Alg b
map f  Zero    = f Zero
map f (v :@ b) = f (v :@ b)
map f (x :+ y) = (map f x) :+ map f y

{-# INLINE filter #-}
-- | filter
filter :: (Alg b -> Bool) -> Alg b -> Alg b
filter f Zero                       = Zero
filter f (v :@ b)   | f (v :@ b)    = v :@ b
                    | otherwise     = Zero
filter f (x :+ y)                   = filter f x .+ filter f y

-- | projection

proj :: (HatBaseClass b) => b -> Alg b -> Alg b
proj b alg = filter (\x ->  x /= Zero && hatBase x == b ) alg

projByAccountTitle :: (ExBaseClass b) => AccountTitles -> Alg b -> Alg b
projByAccountTitle at alg = filter (\x -> x /= Zero &&  (maybeGetAccountTitle . hatBase) x == Just at) alg

projNorm :: (HatBaseClass b) => b -> Alg b -> Number.NonNegative.Double
projNorm b alg  = norm $ (.-)
            $ filter (\x ->  x /= Zero && hatBase x == b ) alg


-- | Baseの大小（==Algの大小）でソート

sort :: (Ord b) => Alg b -> Alg b
sort Zero      = Zero
sort (v :@ b)  = (v :@ b)
sort (x :+ y)  = foldl1 (:+) $ L.sort $ toList (x :+ y)


-- | normの大小でソート
normSort :: Alg b -> Alg b
normSort = undefined

------------------------------------------------------------------
-- * シンプルな基底 増やしたければ増やせる
-- 同じ呼び出し関数を使うためにタプルにしている.
-- DuplicateRecordFields 拡張 よりも制限が少なく 見た目が良いためにこちらを選択
------------------------------------------------------------------
instance BaseClass (HatBase a) where
    autoGetter (h :< b) = autoGetter b
    autoSetter (h :< b) =  let bSetter = autoSetter b
                        in AutoSetter   (\(h :< b) x -> (h :< ) <$> ((_maybeSetAccountTitle bSetter) b x))
                                        (\(h :< b) x -> (h :< ) <$> ((_maybeSetName         bSetter) b x))
                                        (\(h :< b) x -> (h :< ) <$> ((_maybeSetCountunit    bSetter) b x))
                                        (\(h :< b) x -> (h :< ) <$> ((_maybeSetSubject      bSetter) b x))
                                        (\(h :< b) x -> (h :< ) <$> ((_maybeSetDay          bSetter) b x))
                                        (\(h :< b) x -> (h :< ) <$> ((_maybeSetTime         bSetter) b x))

-- ** 要素数 1
-- *** 勘定科目のみ (交換代数基底)

instance BaseClass AccountTitles where
    autoGetter at = initAutoGetter{ _maybeGetAccountTitle = (Just at)}
    autoSetter at = initAutoSetter{ _maybeSetAccountTitle = \at x -> Just x }

instance ExBaseClass (HatBase AccountTitles) where
    getAccountTitle (h :< a)   = a
    setAccountTitle (h :< a) b = h :< b


-- ***  名前のみ(冗長代数基底)
instance BaseClass Name where
    autoGetter name = initAutoGetter{ _maybeGetName = (Just name)}
    autoSetter name = initAutoSetter{ _maybeSetName = \name x -> Just x }

-- *** CountUnitのみ(冗長代数基底)
instance BaseClass CountUnit where
    autoGetter cu = initAutoGetter{ _maybeGetCountUnit = (Just cu)}
    autoSetter cu = initAutoSetter{ _maybeSetCountunit = \cu x -> Just x }

-- *** Dayのみ(冗長代数基底)
instance BaseClass Day where
    autoGetter day = initAutoGetter{ _maybeGetDay = (Just day)}
    autoSetter day = initAutoSetter{ _maybeSetDay = \day x -> Just x }

-- *** TimeOfDayのみ(冗長代数基底)
instance BaseClass TimeOfDay where
    autoGetter time = initAutoGetter{ _maybeGetTime = (Just time)}
    autoSetter time = initAutoSetter{ _maybeSetTime = \time x -> Just x }


-- ** 要素数2

-- | 基礎的なBaseClass 要素数 2
instance BaseClass (AccountTitles, Name) where
    autoGetter (accountTitle, name)
        = initAutoGetter { _maybeGetAccountTitle = Just accountTitle
                         , _maybeGetName         = Just name}

    autoSetter (accountTitle, name)
        = initAutoSetter { _maybeSetAccountTitle = \(accountTitle, name) x -> Just (x, name)
                         , _maybeSetName         = \(accountTitle, name) x -> Just (accountTitle, x)}

instance ExBaseClass (HatBase (AccountTitles, Name)) where
    getAccountTitle (h:< (a, n))   = a
    setAccountTitle (h:< (a, n)) b = h:< (b, n)

-- ** 要素数 3
-- | 基礎的なBaseClass 要素数 3
instance BaseClass (AccountTitles, Name, CountUnit) where
    autoGetter (accountTitle, name, countUnit)
        = initAutoGetter { _maybeGetAccountTitle = Just accountTitle
                         , _maybeGetName         = Just name
                         , _maybeGetCountUnit    = Just countUnit}

    autoSetter (accountTitle, name, countUnit)
        = initAutoSetter { _maybeSetAccountTitle =      \(accountTitle, name, countUnit) x
                                            -> Just (x,            name, countUnit)
                         , _maybeSetName         =      \(accountTitle, name, countUnit) x
                                            -> Just (accountTitle, x,    countUnit)
                         , _maybeSetCountunit    =      \(accountTitle, name, countUnit) x
                                            -> Just (accountTitle, name, x        )}

instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit)) where
    getAccountTitle (h:< (a, n, c))   = a
    setAccountTitle (h:< (a, n, c)) b = h:< (b, n, c)



-- ** 要素数 4
-- | 基礎的なBaseClass 要素数 4
instance BaseClass (AccountTitles, Name, CountUnit, Subject) where
    autoGetter (accountTitle, name, countUnit, subject)
        = initAutoGetter { _maybeGetAccountTitle = Just accountTitle
                         , _maybeGetName         = Just name
                         , _maybeGetCountUnit    = Just countUnit
                         , _maybeGetSubject      = Just subject}

    autoSetter (accountTitle, name, countUnit, subject)
        = initAutoSetter { _maybeSetAccountTitle =      \(accountTitle, name, countUnit, subject) x
                                            -> Just (x,            name, countUnit, subject)
                         , _maybeSetName         =      \(accountTitle, name, countUnit, subject) x
                                            -> Just (accountTitle, x,    countUnit, subject)
                         , _maybeSetCountunit    =      \(accountTitle, name, countUnit, subject) x
                                            -> Just (accountTitle, name, x,         subject)
                         , _maybeSetSubject      =      \(accountTitle, name, countUnit, subject) x
                                            -> Just (accountTitle, name, countUnit, x      )}

instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject)) where
    getAccountTitle (h:< (a, n, c, s))   = a
    setAccountTitle (h:< (a, n, c, s)) b = h:< (b, n, c, s)

-- ** 要素数 5
-- | 基礎的なBaseClass 要素数 5
instance BaseClass (AccountTitles, Name, CountUnit, Subject,  Day) where
    autoGetter (accountTitle, name, countUnit, subject, day)
        = initAutoGetter { _maybeGetAccountTitle = Just accountTitle
                         , _maybeGetName         = Just name
                         , _maybeGetCountUnit    = Just countUnit
                         , _maybeGetSubject      = Just subject
                         , _maybeGetDay          = Just day}

    autoSetter (accountTitle, name, countUnit, subject, day)
        = initAutoSetter { _maybeSetAccountTitle =      \(accountTitle, name, countUnit, subject, day) x
                                            -> Just (x,            name, countUnit, subject, day)
                         , _maybeSetName         =      \(accountTitle, name, countUnit, subject, day) x
                                            -> Just (accountTitle, x,    countUnit, subject, day)
                         , _maybeSetCountunit    =      \(accountTitle, name, countUnit, subject, day) x
                                            -> Just (accountTitle, name, x,         subject, day)
                         , _maybeSetSubject      =      \(accountTitle, name, countUnit, subject, day) x
                                            -> Just (accountTitle, name, countUnit, x,       day)
                         , _maybeSetDay          =      \(accountTitle, name, countUnit, subject, day) x
                                            -> Just (accountTitle, name, countUnit, subject, x  )}

instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject,  Day)) where
    getAccountTitle (h:< (a, n, c, s, d))   = a
    setAccountTitle (h:< (a, n, c, s, d)) b = h:< (b, n, c, s, d)


-- ** 要素数 5
-- | 基礎的なBaseClass 要素数 6
instance BaseClass (AccountTitles, Name, CountUnit, Subject, Day, TimeOfDay) where
    autoGetter (accountTitle, name, countUnit, subject, day, time)
        = initAutoGetter { _maybeGetAccountTitle = Just accountTitle
                         , _maybeGetName         = Just name
                         , _maybeGetCountUnit    = Just countUnit
                         , _maybeGetSubject      = Just subject
                         , _maybeGetDay          = Just day
                         , _maybeGetTime         = Just time}

    autoSetter (accountTitle, name, countUnit, subject, day, time)
        = AutoSetter     { _maybeSetAccountTitle =      \(accountTitle, name, countUnit, subject, day, time) x
                                            -> Just (x,            name, countUnit, subject, day, time)
                         , _maybeSetName         =      \(accountTitle, name, countUnit, subject, day, time) x
                                            -> Just (accountTitle, x,    countUnit, subject, day, time)
                         , _maybeSetCountunit    =      \(accountTitle, name, countUnit, subject, day, time) x
                                            -> Just (accountTitle, name, x,         subject, day, time)
                         , _maybeSetSubject      =      \(accountTitle, name, countUnit, subject, day, time) x
                                            -> Just (accountTitle, name, countUnit, x,       day, time)
                         , _maybeSetDay          =      \(accountTitle, name, countUnit, subject, day, time) x
                                            -> Just (accountTitle, name, countUnit, subject, x,   time)
                         , _maybeSetTime         =      \(accountTitle, name, countUnit, subject, day, time) x
                                            -> Just (accountTitle, name, countUnit, subject, day, x   )}

instance ExBaseClass (HatBase (AccountTitles, Name, CountUnit, Subject, Day, TimeOfDay)) where
    getAccountTitle (h:< (a, n, c, s, d, t))   = a
    setAccountTitle (h:< (a, n, c, s, d, t)) b = h:< (b, n, c, s, d, t)

------------------------------------------------------------------
-- * 基本計算処理
------------------------------------------------------------------
-- ** 振替

-- | 振替変換テーブル
data TransTable b where
     TransTable         :: (HatBaseClass b) => {table :: (Map.Map b {-変換前-} b {-変換後-})}
                                           -> TransTable b
     -- ^ 基底ごと変換
     PartialTransTable  :: (HatBaseClass b) => {partialTable :: (Map.Map b {- 変換前 -} (Number.NonNegative.Double {- 変換量 -}, b {- 変換後 -}))}
                                           -> TransTable b
     -- ^ 部分的に変換
     FunctionTransTable :: (HatBaseClass b) => {functionTable :: (Map.Map b ((Number.NonNegative.Double -> Number.NonNegative.Double), b))}
                                           -> TransTable b
     -- ^ 数値部分に関数を適用して変換 按分はこれを使う



instance Show (TransTable b) where
    show (TransTable b)         = "TransTable "         ++ show b
    show (PartialTransTable b)  = "PartialTransTable "  ++ show b
    show (FunctionTransTable b) = "FunctionTransTable " ++
                                (L.foldl1 (++)
                                (L.map (\(before, (f, after)) -> "(" ++ show before
                                                              ++ ", ( <function:: Number.NonNegative.Double -> Number.NonNegative.Double>, "
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

transfer :: (HatBaseClass b) => TransTable b -> Alg b -> Alg b
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

transfer (FunctionTransTable ms) (v :@ before)
    = case Map.lookup before ms of
        Just (f, after) | (f v) <  0 -> error "transfer: minus value"
                        | (f v) >= 0 -> (v - (rounding (f v))) :@ before
                                     .+      (rounding (f v))  :@ after
                        | otherwise  ->                   v    :@ after
        Nothing         ->                                v    :@ before

transfer tm xs          = map (transfer tm) xs

-- *** 経過勘定の振替

-- ** 仕分け

-- *** 利息の仕分け

{- | 預金利息の仕分け -}

{- | 国債償還
国債発行分だけ利息を払う
    Depo

国債保有分だけ利息をもらう


-}

redemption :: (ExBaseClass b) => NationalBondsInterestRate -> Alg b -> Alg b
redemption _ Zero   = Zero
redemption nbir alg = undefined
    where
    alg' =  (.-) alg
    -- Hat付きの国債及び国債借入金を現金に変換
    hatConvertedAlg :: (ExBaseClass b) => Alg b -> Alg b
    hatConvertedAlg alg = (flip map) alg
                    $ \(v:@ hb) -> case (getHat hb, getAccountTitle hb) of
                                        (Not, _)                    -> (v:@ hb)
                                        (Hat, NationalBonds)        -> v :@ (setAccountTitle hb Cash)
                                        (Hat, NationalBondsPayable) -> v :@ (setAccountTitle hb Cash)


type NationalBondsInterestRate = Prelude.Double
type InterestRate              = Prelude.Double




-- *** 決算振替仕訳

{- | 仕分け -}
accountingJournal = undefined

-- * バランス

{- | バランスしていない場合の処理 -}
forceBalance = undefined


-- * 端数処理

{- | 端数処理
割り算と掛け算に利用
基本的には切り上げで処理する
勘定科目の乗除には全てこれを適用
-}

rounding :: Number.NonNegative.Double -> Number.NonNegative.Double
rounding = fromIntegral . ceiling



-- * AlgSet
------------------------------------------------------------------
type AlgSet = [Alg b]








