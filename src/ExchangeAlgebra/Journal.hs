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
{-# LANGUAGE OverloadedStrings          #-}

module ExchangeAlgebra.Journal
    ( module ExchangeAlgebra.Algebra.Base
    , HatVal(..)
    , HatBaseClass(..)
    , Redundant(..)
    , Exchange(..)
    , pattern (:@)
    , (.@)
    , Note(..)
    , Journal(..)
    , pattern ExchangeAlgebra.Journal.Zero
    , (.|)
    , toAlg
    , fromList
    , sigma
    , sigmaM
    , map
    , insert
    , projWithNote
    , projWithBase
    , projWithNoteBase
    , filterWithNote
    , gather
    )where

import qualified    ExchangeAlgebra.Algebra as EA
import              ExchangeAlgebra.Algebra.Base
import              ExchangeAlgebra.Algebra ( HatVal(..)
                                            , HatBaseClass(..)
                                            , Alg(..)
                                            , Redundant(..)
                                            , Exchange(..)
                                            , pattern (:@)
                                            , (.@))
import              Prelude                 hiding (map, filter)
import qualified    Data.HashMap.Strict as Map
import Control.Parallel.Strategies (using,parTraversable, rdeepseq, NFData)
import qualified Data.Set as S
import Data.Hashable
import qualified Data.Text as T
import qualified Control.Monad as CM

-- | 摘要のクラス
class (Show a, Eq a,Ord a, Hashable a) => Note a where
    plank :: a
    isPlank :: a -> Bool
    isPlank x = x == plank


instance Note String where
    plank = ""

instance Note T.Text where
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
            => {_journal :: Map.HashMap n (Alg v b)} ->  Journal n v b

isZero :: (HatVal v, HatBaseClass b, Note n)
       => Journal n v b -> Bool
-- | Complexity: O(1)
isZero (Journal js) = Map.null js

pattern Zero :: (HatVal v, HatBaseClass b, Note n) => Journal n v b
pattern Zero <- (isZero -> True)
    where
        Zero = Journal Map.empty

-- | smart constructer of :||
-- Complexity: O(1)
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
                    | otherwise = foldr (\x y -> if y == ""
                                              then show x ++ ".|" ++ show k
                                              else y ++ " .+ " ++ show x ++ ".|" ++ show k)
                                        t
                                        (EA.toASCList a)
------------------------------------------------------------------

instance  (HatVal v, HatBaseClass b, Note n) => Semigroup (Journal n v b) where
    {-# INLINE (<>) #-}
    -- | Associative law ;convert to right join
    (<>) = addJournal


-- | (.+) for Jorunal
--
-- >>> type Test = Journal String Double (HatBase AccountTitles)
-- >>> x = 20.00:@Not:<Cash .+ 20.00:@Hat:<Deposits .| "Withdrawal" :: Test
-- >>> y = 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits .| "Deposits" :: Test
-- >>> x .+ y
-- 10.00:@Not:<Deposits.|"Deposits" .+ 10.00:@Hat:<Cash.|"Deposits" .+ 20.00:@Hat:<Deposits.|"Withdrawal" .+ 20.00:@Not:<Cash.|"Withdrawal"


addJournal :: (HatVal v, HatBaseClass b, Note n)
           => Journal n v b -> Journal n v b -> Journal n v b
-- | Complexity: O(jx + jy + overlap-merge-cost)
-- where jx/jy are note counts in each journal.
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
-- | fromList
--
--  >>> type Test = Journal String Double (HatBase AccountTitles)
--  >>> x = [(1.00:@Hat:<Cash .| z) |  z <- ["Loan Payment","Purchace Apple"]] :: [Test]
--  >>> fromList x
--  1.00:@Hat:<Cash.|"Loan Payment" .+ 1.00:@Hat:<Cash.|"Purchace Apple"

fromList :: (HatVal v, HatBaseClass b, Note n)
         => [Journal n v b] -> Journal n v b
-- | Complexity: O(sum of addJournal costs)
fromList = foldr (.+) mempty

------------------------------------------------------------------
-- | sigma
-- Complexity: O(sum of addJournal costs over generated terms)
sigma :: (HatVal v, HatBaseClass b, Note n) => [a] -> (a -> Journal n v b) -> Journal n v b
sigma xs f = foldr ((.+) . f) mempty xs

-- | Complexity: O(m + monoid-combine-cost), where m is input length.
sigmaM :: (Monoid m, Monad m0) => [a] -> (a -> m0 m) -> m0 m
sigmaM xs f = mconcat <$> CM.forM xs f


------------------------------------------------------------------
toAlg :: (HatVal v, HatBaseClass b, Note n)
      => Journal n v b -> Alg v b
-- | Complexity: O(j + sum of Alg union costs), where j is note count.
toAlg (Journal js) = Map.foldl' (.+) EA.Zero js

------------------------------------------------------------------
map :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
-- | Complexity: O(j * cost(f)), where j is note count.
map f (Journal js) = Journal (Map.map f js)


parallelMap :: (NFData b, Ord k) => (a -> b) -> Map.HashMap k a -> Map.HashMap k b
-- | Complexity: O(j * cost(f)) total work; wall-clock may improve with parallelism.
parallelMap f m = Map.map f m `using` parTraversable rdeepseq

parMap :: (HatVal v, HatBaseClass b, Note n)
    => (Alg v b -> Alg v b) -> Journal n v b -> Journal n v b
-- | Complexity: O(j * cost(f)) total work; parallel scheduling overhead applies.
parMap f (Journal js) = Journal (parallelMap f js)

-- | insert
--  "insert x y" inserts x to y. if note already exit in y, replace previous(y) by x.
-- >>> type Test = Journal String Double (HatBase AccountTitles)
-- >>> x = 10.00:@Not:<Cash .| "A" :: Test
-- >>> y = 20.00:@Not:<Cash .| "B" :: Test
-- >>> z = 30.00:@Hat:<Cash .| "A" :: Test
-- >>> insert z (x .+ y)
-- 30.00:@Hat:<Cash.|"A" .+ 20.00:@Not:<Cash.|"B"

insert :: (HatVal v, HatBaseClass b, Note n)
        => Journal n v b -> Journal n v b -> Journal n v b
-- | Complexity: O(jx + jy) expected, based on hash-map union.
insert (Journal xs) (Journal ys) = Journal (Map.union xs ys)


------------------------------------------------------------------
-- | projWithNote
-- Projecting with Note.
--  >>> type Test = Journal String Double (HatBase CountUnit)
--  >>> x = 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .| "cat"  :: Test
--  >>> y = 2.00:@Hat:<Yen .+ 2.00:@Not:<Amount .| "dog"  :: Test
--  >>> z = 3.00:@Hat:<Yen .+ 3.00:@Not:<Amount .| "fish" :: Test
--  >>> projWithNote ["dog","cat"] (x .+ y .+ z)
--  2.00:@Not:<Amount.|"dog" .+ 2.00:@Hat:<Yen.|"dog" .+ 1.00:@Not:<Amount.|"cat" .+ 1.00:@Hat:<Yen.|"cat"

projWithNote :: (HatVal v, HatBaseClass b, Note n)
             => [n] -> Journal n v b -> Journal n v b
-- | Complexity:
--   - wildcard note present: O(1)
--   - single note: expected O(1)
--   - multi-note: O(p + j), where p is filter-list length and j is note count.
projWithNote ns (Journal js)
    | any isPlank ns = Journal js
projWithNote [n] (Journal js) = Journal $ case Map.lookup n js of
    Nothing -> Map.empty
    Just a  -> Map.singleton n a
projWithNote ns (Journal js) = Journal $ Map.filterWithKey (\k _ -> S.member k nsSet) js
  where
    nsSet = S.fromList ns
------------------------------------------------------------------
-- | projWithBase
-- Projecting with Base.
--  >>> type Test = Journal String Double (HatBase CountUnit)
--  >>> x = 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .| "cat"  :: Test
--  >>> y = 2.00:@Not:<Yen .+ 2.00:@Hat:<Amount .| "dog"  :: Test
--  >>> z = 3.00:@Hat:<Yen .+ 3.00:@Not:<Amount .| "fish" :: Test
--  >>> projWithBase [Not:<Amount] (x .+ y .+ z)
--  3.00:@Not:<Amount.|"fish" .+ 1.00:@Not:<Amount.|"cat"

projWithBase :: (HatVal v, HatBaseClass b, Note n)
             => [b] -> Journal n v b -> Journal n v b
-- | Complexity: O(j * cost(EA.proj bs)), where j is note count.
projWithBase [] _ = mempty
projWithBase xs js = map (EA.proj xs) js

------------------------------------------------------------------
-- | projFromJournal
-- Projecting with Note and Base.
--  >>> type Test = Journal String Double (HatBase CountUnit)
--  >>> x = 1.00:@Hat:<Yen .+ 1.00:@Not:<Amount .| "cat"  :: Test
--  >>> y = 2.00:@Not:<Yen .+ 2.00:@Hat:<Amount .| "dog"  :: Test
--  >>> z = 3.00:@Hat:<Yen .+ 3.00:@Not:<Amount .| "fish" :: Test
--  >>> projWithNoteBase ["dog","fish"] [Not:<Amount] (x .+ y .+ z)
--  3.00:@Not:<Amount.|"fish"

projWithNoteBase :: (HatVal v, HatBaseClass b, Note n)
                 => [n] -> [b] -> Journal n v b -> Journal n v b
-- | Complexity: O(cost(projWithNote) + selected-notes * cost(EA.proj bs)).
projWithNoteBase _ [] _ = mempty
projWithNoteBase ns bs js | any isPlank ns = projWithBase bs js
projWithNoteBase [n] bs (Journal js) = Journal $ case Map.lookup n js of
    Nothing -> Map.empty
    Just a  -> Map.singleton n (EA.proj bs a)
projWithNoteBase [] bs js = projWithBase bs js
projWithNoteBase ns bs js = projWithBase bs $ projWithNote ns js

------------------------------------------------------------------
filterWithNote :: (HatVal v, HatBaseClass b, Note n)
             => (n -> Alg v b -> Bool) -> Journal n v b -> Journal n v b
-- | Complexity: O(j), where j is note count.
filterWithNote f (Journal js) = Journal (Map.filterWithKey f js)

------------------------------------------------------------------
-- | gather
-- Gathers all Alg into one on the given Note.
--
-- >>> type Test = Journal String Double (EA.HatBase EA.AccountTitles)
-- >>> x = 20.00:@Not:<Cash .+ 20.00:@Hat:<Deposits .| "Withdrawal" :: Test
-- >>> y = 10.00:@Hat:<Cash .+ 10.00:@Not:<Deposits .| "Deposits" :: Test
-- >>> gather "A" (x .+ y)
-- 10.00:@Not:<Deposits.|"A" .+ 20.00:@Hat:<Deposits.|"A" .+ 20.00:@Not:<Cash.|"A" .+ 10.00:@Hat:<Cash.|"A"

gather :: (HatVal v, HatBaseClass b, Note n)
       => n -> Journal n v b -> Journal n v b
-- | Complexity: O(cost(toAlg js))
gather n js = (toAlg js) .| n








