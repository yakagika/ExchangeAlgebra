
{-# LANGUAGE  MultiParamTypeClasses
            , DuplicateRecordFields
            , TypeSynonymInstances
            , IncoherentInstances
            , DeriveDataTypeable
            , FlexibleInstances
            , TypeOperators
            , BangPatterns
            , InstanceSigs  
            , TypeFamilies
            , RankNTypes
            , GADTs               #-}


module ExchangeAlgebraExtensible where

import              Debug.Trace
import qualified    Data.Text       as T
import              Data.Text       (Text)
import qualified    Data.List       as L (map, length, elem,sort,foldl1,filter)
import              Prelude         hiding (map, head, filter,tail)




-----------------------------------------------
-- Define Base
-----------------------------------------------

class (Ord a, Show a, Eq a) => BeName a where

class (Ord a, Show a, Eq a) => BeUnit a where

class (Ord a, Show a, Eq a) => BeSubject a where

class (Show a, Eq a, Ord a) => Elm a where
    labels  :: a -> [String]
    elmnum  :: a -> Int    

class (Show a, Eq a, Ord a) => Base a where
    getHat :: a -> Hat
    revHat :: a -> a
    isHat  :: a -> Bool

data Hat = Hat | Not deriving (Ord, Eq)

instance Show Hat where
    show Hat = "^"
    show Not = ""

data Unit u where
     Unit :: (BeUnit u) =>  {unit :: u} -> Unit u

instance Eq (Unit u) where
    (Unit n) == (Unit n') = n == n'

instance Ord (Unit u) where 

instance (Show u) => Show (Unit u) where
    show (Unit u) = show u

instance (BeUnit u) =>  Elm (Unit u) where
    labels (Unit u) = ["unit"]
    elmnum (Unit u) = 1

data TBase h e where
    (:<<) :: (Elm e) => {hat :: !Hat, elm :: !e} -> TBase Hat e
    
instance (Show h, Show e) => Show (TBase h e) where
    show (h :<< e) = show h ++ ":<" ++ show e

type UB = TBase Hat (Unit CountUnit)
instance Base UB where
    getHat (h :<< x) = h
    revHat (Hat :<< x) = (Not :<< x)
    revHat (Not :<< x) = (Hat :<< x)
    isHat  (Hat :<< _) = True
    isHat  (Not :<< _) = False


------------------------------------------------------------
-- Reducdunt　これを継承すれば冗長代数になる
------------------------------------------------------------
class Redundant a where
    (.^) :: a -> a
    (.~) :: a -> a
    norm :: a -> Double


------------------------------------------------------------
-- Redundant Algebra
------------------------------------------------------------
infixr 7 :@
infixr 5 :+


{-@ type R = {i:Int | i >= 0 }@-}

data Alg b where
    Zero :: Alg b
    {-@ (:@) ::  (Base b) => {val :: !R, base :: !b}  -> Alg b @-}
    (:@) :: (Base b) => {val :: !Double, base :: !b}  -> Alg b
    (:+) :: (Base b) => !(Alg b) -> !(Alg b) -> Alg b

instance (Show b) => Show (Alg b) where
    show Zero           = "0"
    show (v :@ b)       = (show v) ++ ":@" ++  (show b)
    show (x :+ y)       = (show x) ++ ".+ " ++ (show y) 

instance (Base b, Ord b, Eq b) => Eq (Alg b) where
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _    Zero = False

    (==) (v :@ b) (v' :@ b') 
        | v == v' && b == b'    = True
        | otherwise             = False 

    (==) x y = f x == f y
        where f = (L.filter (Zero <)) . L.sort . toList

    (/=) x y = not (x == y)  


instance (Base b, Ord b) => Ord (Alg b) where
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


instance (Base b) => Semigroup (Alg b) where
    (<>) Zero Zero  = Zero
    (<>) Zero !x     = x
    (<>) !x    Zero  = x
    (<>) !(v :@ b) !(v' :@ b') = (v :@ b) :+ (v' :@ b')
    (<>) !x  !y =  f ( x :+ y)
        where f = (foldl1 (:+)) . (L.filter (/= Zero)) . toList 

instance (Base b) => Monoid (Alg b) where
    mempty = Zero
    mappend = (<>)
    mconcat []       = Zero
    mconcat [Zero]   = Zero
    mconcat [v :@ b] = v :@ b
    mconcat (x:y) = x `mappend`  mconcat y


-- :+ の　Evaluationに相当する
infixr 5 .+
(.+) :: (Base b) => Alg b -> Alg b -> Alg b
(.+) = mappend

instance (Base b) =>  Redundant (Alg b) where
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
                case (h,h') of
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
            g :: Alg b -> Bool
            g Zero     = False
            g (0 :@ _) = False
            g _        = True  

            z = sort xs
            f :: (Base b) => Alg b  -> Alg b
            f Zero       = Zero
            f (v :@ b)   | v == 0.0  = Zero
                         | otherwise = v :@ b
            f ((v :@ b) :+ (v' :@ b'))  = (.~) ((v :@ b) :+ (v' :@ b')) 
            f xs    | isZero h1              = f t 
                    | base h1 /= base h2     = h1 :+ f t
                    | otherwise = f $ (f (h1 :+ h2)) :+ tail t 
                    where
                        t  = tail xs
                        h1 = head xs
                        h2 = head t

-- 基本の関数
vals :: Alg b -> [Double]
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

fromList :: (Base b) =>  [Alg b] -> Alg b
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

map ::   (Alg b -> Alg b) -> Alg b -> Alg b
map f  Zero    = f Zero
map f (v :@ b) = f (v :@ b)
map f (x :+ y) = (map f x) :+ map f y 

filter :: (Alg b -> Bool) -> Alg b -> [Alg b]
filter f Zero       | f Zero        = [Zero]
                    | otherwise     = []
filter f (v :@ b)   | f (v :@ b)    = [v :@ b]
                    | otherwise     = []
filter f (x :+ y) = filter f x ++ filter f y

-- projection 

proj :: (Base b) => b -> Alg b -> Alg b
proj b alg = fromList $ filter (\x ->  x /= Zero && base x == b ) alg 

projNorm :: (Base b) => b -> Alg b -> Double
projNorm b alg  = norm $ (.~) 
            $ fromList 
            $ filter (\x ->  x /= Zero && base x == b ) alg 

-- Transfer
transfer :: (Base b) => b -> b -> Alg b -> Alg b
transfer _  _  Zero = Zero 
transfer b1 b2 (v :@ b) | b == b1 = v :@ b1
                        | otherwise = v :@ b
transfer b1 b2 xs = map (transfer b1 b2) xs



-- Baseの大小（==Algの大小）でソート

sort :: (Ord b) => Alg b -> Alg b
sort Zero      = Zero
sort (v :@ b)  = (v :@ b)
sort (x :+ y)  = foldl1 (:+) $ L.sort $ toList (x :+ y)

 
-- normの大小でソート
normSort :: Alg b -> Alg b
normSort = undefined


------------------------------------------------------------
-- Exchange これを継承すれば交換代数になる
------------------------------------------------------------
class (Redundant a) => Exchange a where
--    subject :: a -> AccountTitles
    unit    :: a -> CountUnit
    -- R-L decomposition
    decR :: a -> a 
    decL :: a -> a
    -- P-M decomposition
    decP :: a -> a 
    decM :: a -> a
    -- norm Balance
    balance :: a -> Bool

------------------------------------------------------------
-- Define ExBase 
------------------------------------------------------------
class (Base a) => ExBase a where
    whatDiv     :: a -> AccountDivision
    whatPIMO    :: a -> PIMO
    whichSide   :: a -> Side

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
-- Exchange Algbra Bases Elements
------------------------------------------------------------

                    -- Assets
data  AccountTitles =    Cash
                    |    Deposits
                    |    NationalBonds
                    |    Products { detail :: Text}
                    |    StockInvectment
                    |    EquipmentInvestment
                    |    LoansReceivable
                    |    ReserveDepositReceivable
                    |    Gold
                    |    GovernmentService
                    -- Equity
                    |    CapitalStock 
                    |    RetainedEarnings -- ここに取り敢えず入れておく,要確認
                    -- Liability
                    |    LoansPayable
                    |    ReserveForDepreciation
                    |    DepositPayable
                    |    NationalBondsPayable
                    |    ReserveDepositPayable
                    |    CentralBankNotePayable
                    -- Cost
                    |    Depreciation
                    |    WageExpenditure
                    |    InterestExpense
                    |    TaxesExpense
                    |    ConsumptionExpenditure
                    |    SubsidyExpense
                    |    CentralBankPaymentExpence
                    -- Revenue
                    |    ValueAdded
                    |    SubsidyIncome
                    |    NationalBondInterestEarned
                    |    DepositInterestEarned
                    |    GrossProfit
                    |    OrdinaryProfit
                    |    InterestEarned 
                    |    WageEarned
                    |    TaxesRevenue
                    |    CentralBankPaymentIncome
                    deriving (Show,Eq)


instance BeSubject    AccountTitles   where
instance Enum       AccountTitles   where
    fromEnum    Cash                        = 0
    fromEnum    Deposits                    = 1
    fromEnum    NationalBonds               = 2
    fromEnum    (Products _ )               = 3
    fromEnum    StockInvectment             = 4
    fromEnum    EquipmentInvestment         = 5
    fromEnum    LoansReceivable             = 6
    fromEnum    ReserveDepositReceivable    = 7
    fromEnum    Gold                        = 8
    fromEnum    GovernmentService           = 9
    fromEnum    CapitalStock                = 10
    fromEnum    LoansPayable                = 11
    fromEnum    ReserveForDepreciation      = 12
    fromEnum    DepositPayable              = 13
    fromEnum    NationalBondsPayable        = 14
    fromEnum    ReserveDepositPayable       = 15
    fromEnum    CentralBankNotePayable      = 16
    fromEnum    Depreciation                = 17
    fromEnum    WageExpenditure             = 18
    fromEnum    InterestExpense             = 19
    fromEnum    TaxesExpense                = 20
    fromEnum    ConsumptionExpenditure      = 21
    fromEnum    SubsidyExpense              = 22
    fromEnum    CentralBankPaymentExpence   = 23
    fromEnum    ValueAdded                  = 24
    fromEnum    RetainedEarnings            = 25
    fromEnum    SubsidyIncome               = 26
    fromEnum    NationalBondInterestEarned  = 27
    fromEnum    DepositInterestEarned       = 28
    fromEnum    GrossProfit                 = 29
    fromEnum    OrdinaryProfit              = 30
    fromEnum    InterestEarned              = 31
    fromEnum    WageEarned                  = 32
    fromEnum    TaxesRevenue                = 33
    fromEnum    CentralBankPaymentIncome    = 34

    toEnum 0    = Cash                       
    toEnum 1    = Deposits                    
    toEnum 2    = NationalBonds               
    toEnum 3    = Products T.empty                  
    toEnum 4    = StockInvectment             
    toEnum 5    = EquipmentInvestment         
    toEnum 6    = LoansReceivable             
    toEnum 7    = ReserveDepositReceivable    
    toEnum 8    = Gold                        
    toEnum 9    = GovernmentService           
    toEnum 10   = CapitalStock                
    toEnum 11   = LoansPayable                
    toEnum 12   = ReserveForDepreciation      
    toEnum 13   = DepositPayable              
    toEnum 14   = NationalBondsPayable        
    toEnum 15   = ReserveDepositPayable       
    toEnum 16   = CentralBankNotePayable      
    toEnum 17   = Depreciation                
    toEnum 18   = WageExpenditure             
    toEnum 19   = InterestExpense             
    toEnum 20   = TaxesExpense                
    toEnum 21   = ConsumptionExpenditure      
    toEnum 22   = SubsidyExpense              
    toEnum 23   = CentralBankPaymentExpence   
    toEnum 24   = ValueAdded                  
    toEnum 25   = RetainedEarnings            
    toEnum 26   = SubsidyIncome                
    toEnum 27   = NationalBondInterestEarned  
    toEnum 28   = DepositInterestEarned       
    toEnum 29   = GrossProfit                 
    toEnum 30   = OrdinaryProfit              
    toEnum 31   = InterestEarned              
    toEnum 32   = WageEarned                  
    toEnum 33   = TaxesRevenue               
    toEnum 34   = CentralBankPaymentIncome    


instance Ord AccountTitles where
    compare (Products x) (Products y) = compare x y
    compare x y = compare (fromEnum x) (fromEnum y)
    
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


data  CountUnit = Yen | Amount deriving (Ord, Show,Eq)
instance BeUnit CountUnit where
instance BeName Text where


------------------------------------------------------------------
-- 以下,実構造の設計
------------------------------------------------------------------
------------------------------------------------------------------
-- Simplest Redundant Algebra
------------------------------------------------------------------

-- HatだけでRedandunt　Algebra は作れる
instance Base Hat where
    getHat  x   = x
    revHat Hat  = Not
    revHat Not  = Hat
    isHat  Hat   = True
    isHat _     = False

type JustRedundant = Alg Hat


------------------------------------------------------------------
-- BeSubject だけの Base
------------------------------------------------------------------

data Base1 h s where
    SBase :: (BeSubject u) => Hat -> u -> Base1 Hat u

{-  instance Base (Base1 h s) where
    getHat  x   = x
    revHat Hat  = Not
    revHat Not  = Hat
    isHat  Hat   = True
    isHat _     = False
-}

-----------------------------------------------
-- Multi Dimention Base 
-----------------------------------------------

-- Base も一般化して，otherの部分を与える


infixr 8 .<  
(.<) = (,)

infixr 7 :<
data MDBase　h u s where 
     (:<) :: (BeUnit u, BeSubject s)  
            => !Hat ->  !(u, s) ->  MDBase Hat u s


instance (BeUnit b, BeSubject c) => Eq (MDBase h b c) where
    (==) (h1 :< (u1,s1))(h2 :< (u2, s2))
        | u1 == u2 && s1 == s2 = True
        | otherwise = False

    (/=) x y = not (x == y)

instance Show (MDBase h a b) where
    show (h :<(a,b)) = (show h) 
                        ++ "<" 
                        ++ (show a) 
                        ++ ", " 
                        ++ (show b) 
                        ++ " >"    

instance (BeUnit u, BeSubject s) => Base (MDBase Hat u s) where
    getHat (h:<(u,s)) = h
    revHat (Hat:< x) = Not :< x
    revHat (Not:< x) = Hat :< x
    isHat x | getHat x == Hat = True
            | otherwise       = False

instance (BeUnit u, BeSubject s) => Ord (MDBase Hat u s) where
    compare !(h :< (u, s)) !(h' :<(u', s'))
        | s >  s' = GT
        | s == s'  && u >  u' = GT
        | s == s'  && u == u' = EQ
        | s == s'  && u <  u' = LT
        | s <  s'  = LT 

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


 
type ExMDBase = MDBase Hat CountUnit AccountTitles

instance ExBase ExMDBase where
    whatDiv (_ :< (_,s)) 
        | s == CapitalStock || s == RetainedEarnings = Equity
        | L.elem s  [ LoansPayable
                    , ReserveForDepreciation
                    , DepositPayable
                    , NationalBondsPayable
                    , ReserveDepositPayable
                    , CentralBankNotePayable] 
                    = Liability
        | L.elem s  [ Depreciation
                    , WageExpenditure
                    , InterestExpense
                    , TaxesExpense
                    , ConsumptionExpenditure
                    , SubsidyExpense
                    , CentralBankPaymentExpence]
                    = Cost
        | L.elem s  [ ValueAdded
                    , SubsidyIncome
                    , NationalBondInterestEarned
                    , DepositInterestEarned
                    , GrossProfit
                    , OrdinaryProfit
                    , InterestEarned
                    , WageEarned
                    , TaxesRevenue
                    , CentralBankPaymentIncome]
                    = Revenue
        | otherwise = Assets
    whatPIMO x
        | whatDiv x == Assets       = PS
        | whatDiv x == Equity       = MS
        | whatDiv x == Liability    = MS
        | whatDiv x == Cost         = OUT
        | whatDiv x == Revenue      = IN

    whichSide x 
        | getHat x == Hat  = f $ whatDiv x
        | otherwise     = switchSide $ f $ whatDiv x 
        where
            f Assets    = Credit
            f Cost      = Credit
            f Liability = Debit
            f Equity    = Debit
            f Revenue   = Debit

------------------------------------------------------------
-- 要素が2つの(最小の)Exchange Algebra
------------------------------------------------------------
-- Basic Exchange Algebra 中身が違うものも作れるけどとりあえず代表的なもの
type Ex2Base     = MDBase Hat CountUnit AccountTitles
type Ex2Alg      = Alg Ex2Base

instance Exchange Ex2Alg where
--    subject ( _:@ _ :< (u,s)) = s
    unit    ( _:@ _ :< (u,s)) = u
    decR xs = fromList $ filter (\x -> x /= Zero && (whichSide . base) x == Debit) xs
    decL xs = fromList $ filter (\x -> x /= Zero && (whichSide . base) x == Credit) xs
    decP xs = fromList $ filter (\x -> x /= Zero && (isHat . base ) x) xs
    decM xs = fromList $ filter (\x -> x /= Zero && (not. isHat. base) x) xs
    balance xs  | (norm . decR) xs == (norm . decL) xs = True
                | otherwise                            = False
 




