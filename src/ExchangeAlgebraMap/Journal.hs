{- |
    Module     : ExchangeAlgebra.Record
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
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module ExchangeAlgebraMap.Journal
    ( module ExchangeAlgebraMap.Algebra.Base
    , HatVal(..)
    , HatBaseClass(..)
    , Redundant(..)
    , Exchange(..)
    , pattern (:@)
    , Note(..)
    , Journal(..)
    , pattern ExchangeAlgebraMap.Journal.Zero
    , (.|)
    , toAlg
    , fromList
    , map
    , projWithNote
    , projWithBase
    , projWithNoteBase
    , filterWithNote
    , gather
    )where

import qualified    ExchangeAlgebraMap.Algebra as EA
import              ExchangeAlgebraMap.Algebra.Base
import              ExchangeAlgebraMap.Algebra ( HatVal(..)
                                               , HatBaseClass(..)
                                               , Alg(..)
                                               , Redundant(..)
                                               , Exchange(..)
                                               , pattern (:@))
import qualified    Data.List               as L    ( foldr1
                                                    , foldr
                                                    , map
                                                    , length
                                                    , elem
                                                    , sort
                                                    , foldl1
                                                    , filter
                                                    , or
                                                    , and
                                                    , sum)
import              Prelude                 hiding (map, head, filter,tail, traverse, mapM)
import qualified    Data.Map.Strict as Map


-- | 摘要のクラス
class (Show a, Eq a,Ord a) => Note a where
    plank :: a
    isPlank :: a -> Bool
    isPlank x = x == plank

instance Note String where
    plank = ""

instance (Note a, Note b) => Note (a,b) where
    plank = (plank, plank)

instance (Note a, Note b, Note c) => Note (a,b,c) where
    plank = (plank, plank, plank)

instance (Note a, Note b, Note c, Note d) => Note (a,b,c,d) where
    plank = (plank, plank, plank, plank)


-- | 摘要の付随した取引データ
data Journal n v b where
     Journal :: (Note n, HatVal v, HatBaseClass b)
            => {_journal :: Map.Map n (Alg v b)} ->  Journal n v b

isZero :: (HatVal v, HatBaseClass b, Note n)
       => Journal n v b -> Bool
isZero (Journal js) | null js   = True
                    | otherwise = False

pattern Zero :: (HatVal v, HatBaseClass b, Note n) => Journal n v b
pattern Zero <- (isZero -> True)
    where
        Zero = Journal Map.empty

-- | smart constructer of :||
(.|) :: (HatVal v, HatBaseClass b, Note n)
      => Alg v b -> n -> Journal n v b
(.|) alg n = Journal (Map.singleton n alg)


infixr 2 .|

------------------------------------------------------------------
-- Show
------------------------------------------------------------------
instance (HatVal v, HatBaseClass b, Note n) => Show (Journal n v b) where
    -- Noteの付いていない要素はplankとして記録されている前提
    show (Journal js) | null js = "0"
                      | otherwise = Map.foldrWithKey f "" js
        where
            f k a t | isPlank k = if t == ""
                                    then show a
                                    else t ++ " .+ " ++ show a
                    | otherwise = L.foldr (\x y -> if y == ""
                                                then show x ++ ".|" ++ show k
                                                else y ++ " .+ " ++ show x ++ ".|" ++ show k)
                                          t
                                          (EA.toList a)
------------------------------------------------------------------

instance  (HatVal v, HatBaseClass b, Note n) => Semigroup (Journal n v b) where
    {-# INLINE (<>) #-}
    -- | Associative law ;convert to right join
    (<>) = addJournal


-- | (.+) for Jorunal
--
-- >>> type Test = Journal String Double (EA.HatBase EA.AccountTitles)
-- >>> x = 20.00:@Not:<Cash .+ 20.00:@Hat:<Deposits .| "Withdrawal" :: Test
-- >>> y = 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits .| "Deposits" :: Test
-- >>> x .+ y
-- 20.00:@Hat:<Deposits.|"Withdrawal" .+ 20.00:@Not:<Cash.|"Withdrawal" .+ 10.00:@Not:<Deposits.|"Deposits" .+ 10.00:@Hat:<Cash.|"Deposits"

addJournal :: (HatVal v, HatBaseClass b, Note n)
           => Journal n v b -> Journal n v b -> Journal n v b
addJournal (Journal xs) (Journal ys) = Journal (Map.unionWith (.+) xs ys)


instance (HatVal v, HatBaseClass b, Note n) => Monoid (Journal n v b) where
    -- 単位元
    mempty = Journal Map.empty
    mappend = (<>)

instance (HatVal v, HatBaseClass b, Note n) => Redundant (Journal n) v b where
    (.^) = map (.^)

    (.+) = mappend

    (.*) x  = map ((.*) x)

    norm = norm . toAlg

    (.-) x = map (.-) (gather plank x)

    compress = map compress


instance (Note n, HatVal v, ExBaseClass b) =>  Exchange (Journal n) v b where
    -- | filter Debit side
    decR js = map (EA.filter (\x -> x /= EA.Zero && (whichSide . EA._hatBase) x == Debit)) js

    -- | filter Credit side
    decL xs = map (EA.filter (\x -> x /= EA.Zero && (whichSide . EA._hatBase) x == Credit)) xs

    -- | filter Plus Stock
    decP xs = map (EA.filter (\x -> x /= EA.Zero && (isHat . EA._hatBase ) x)) xs

    -- | filter Minus Stock
    decM xs = map (EA.filter (\x -> x /= EA.Zero && (not. isHat. EA._hatBase) x)) xs

    -- | check Credit Debit balance
    balance xs  | (norm . decR) xs == (norm . decL) xs = True
                | otherwise                            = False

    -- |
    diffRL xs  | r > l = (Debit, r - l)
               | l > r = (Credit, l -r)
               | otherwise = (Side,0)
        where
        r = (norm . decR) xs
        l = (norm . decL) xs


------------------------------------------------------------------
fromList :: (HatVal v, HatBaseClass b, Note n)
         => [Journal n v b] -> Journal n v b
fromList []     = mempty
fromList [x]    = x
fromList (x:xs) = x .+ (fromList xs)
------------------------------------------------------------------
toAlg :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Alg v b
toAlg = EA.fromList . Map.elems . _journal

------------------------------------------------------------------
map :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
map f (Journal js) = Journal (Map.map f js)


------------------------------------------------------------------
-- | projWithNote
-- Projecting with Note.
--  >>> type Test = Journal String Double (HatBase CountUnit)
--  >>> x = 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .| "cat"  :: Test
--  >>> y = 2.00:@Hat:<Yen .+ 2.00:@Not:<Amount .| "dog"  :: Test
--  >>> z = 3.00:@Hat:<Yen .+ 3.00:@Not:<Amount .| "fish" :: Test
--  >>> projWithNote ["dog","cat"] (x .+ y .+ z)
--  2.00:@Hat:<Yen .+ 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .+ 2.00:@Not:<Amount

projWithNote :: (HatVal v, HatBaseClass b, Note n)
             => [n] -> Journal n v b -> Journal n v b
projWithNote [] _ = mempty
projWithNote [n] (Journal js) = case Map.lookup n js of
                                Nothing -> mempty
                                Just x  -> x .| n
projWithNote (x:xs) js = (.+) (projWithNote [x] js) (projWithNote xs js)

------------------------------------------------------------------
projWithBase :: (HatVal v, HatBaseClass b, Note n)
             => [b] -> Journal n v b -> Journal n v b
projWithBase [] js      = mempty
projWithBase [b] js     = map (EA.proj [b]) js
projWithBase (b:bs) js  = (.+) (projWithBase [b] js) (projWithBase bs js)

------------------------------------------------------------------
-- | projFromJournal
-- Projecting with Note and Base.
--  >>> type Test = Journal String Double (HatBase CountUnit)
--  >>> x = 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .| "cat"  :: Test
--  >>> y = 2.00:@Not:<Yen .+ 2.00:@Hat:<Amount .| "dog"  :: Test
--  >>> z = 3.00:@Hat:<Yen .+ 3.00:@Not:<Amount .| "fish" :: Test
--  >>> projWithNoteBase ["dog","fish"] [Not:<Amount] (x .+ y .+ z)
--  3.00:@Not:<Amount

projWithNoteBase :: (HatVal v, HatBaseClass b, Note n)
                 => [n] -> [b] -> Journal n v b -> Journal n v b
projWithNoteBase [] [] js = mempty
projWithNoteBase [] bs js = projWithBase bs js
projWithNoteBase ns bs js = projWithBase bs $ projWithNote ns js

------------------------------------------------------------------
filterWithNote :: (HatVal v, HatBaseClass b, Note n)
             => (n -> Alg v b -> Bool) -> Journal n v b -> Journal n v b
filterWithNote f (Journal js) = Journal (Map.filterWithKey f js)

------------------------------------------------------------------
-- | gather
-- Gathers all Alg into one on the given Note.
--
-- >>> type Test = Journal String Double (EA.HatBase EA.AccountTitles)
-- >>> x = 20.00:@Not:<Cash .+ 20.00:@Hat:<Deposits .| "Withdrawal" :: Test
-- >>> y = 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits .| "Deposits" :: Test
-- >>> gather "A" (x .+ y)
-- 20.00:@Hat:<Deposits.|"A" .+ 20.00:@Not:<Cash.|"A" .+ 10.00:@Not:<Deposits.|"A" .+ 10.00:@Hat:<Cash.|"A"
gather :: (HatVal v, HatBaseClass b, Note n)
       => n -> Journal n v b -> Journal n v b
gather n js = (toAlg js) .| n















