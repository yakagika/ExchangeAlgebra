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

module ExchangeAlgebra.Record where

import qualified    ExchangeAlgebra.Algebra as EAlg
import              ExchangeAlgebra.Algebra
import qualified    Data.List               as L    ( foldr1
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



-- | 摘要のクラス
class (Show a, Eq a) => Summary a where
    plank :: a
    isPlank :: a -> Bool
    isPlank x = x == plank

instance Summary String where
    plank = ""

-- | 摘要の付随した取引データ
data  Record n b a where
    (:||)   :: (HatVal n, HatBaseClass b, Summary a)
            => {_alg :: Alg n b, _summary :: a} -> Record n b a



infixr 2 :||

instance (HatVal n, HatBaseClass b, Summary a) => Show (Record n b a) where
    show (alg :|| s)
        | isPlank s = " ||: " ++ show alg ++ " :|| " ++ "\"\""
        | otherwise = " ||: " ++ show alg ++ " :|| " ++ show s

instance (HatVal n, HatBaseClass b, Summary a) => Eq (Record n b a) where
    (==) (a1 :|| s1) (a2 :|| s2) = a1 == a2 && s1 == s2


fromRecord :: (HatVal n, HatBaseClass b, Summary a)
           => [Record n b a] -> Alg n b
fromRecord xs   = EAlg.fromList
                $ (L.map _alg xs)

filterRecord :: (HatVal n, HatBaseClass b, Summary a)
             => (Record n b a -> Bool) -> [Record n b a] -> [Record n b a]
filterRecord = L.filter

-- | Summary による抽出
--  >>> import qualified Number.NonNegative as NN
--  >>> type Test = Record NN.Double (HatBase CountUnit) [Char]
--  >>> x = 1:@Hat:<Yen .+ 1:@Not:<Amount :|| "cat"  :: Test
--  >>> y = 2:@Hat:<Yen .+ 2:@Not:<Amount :|| "dog"  :: Test
--  >>> z = 3:@Hat:<Yen .+ 3:@Not:<Amount :|| "fish" :: Test
--  >>> projRecord ["dog","cat"] [x,y,z]
--  1.0:@Hat:<Yen .+ 1.0:@Not:<Amount .+ 2.0:@Hat:<Yen .+ 2.0:@Not:<Amount

projRecord :: (HatVal n, HatBaseClass b, Summary a)
           => (Summary a) => [a] -> [Record n b a] -> Alg n b
projRecord []   _   = Zero
------------------------------------------------------------------
projRecord _    []  = Zero
------------------------------------------------------------------
projRecord [s]  [r] | s ==  _summary r = _alg r
                    | otherwise        = Zero
------------------------------------------------------------------
projRecord ss   [r] | L.or (L.map (\s -> s == _summary r) ss) = _alg r
                    | otherwise                               = Zero
------------------------------------------------------------------
projRecord ss rs = fromRecord
                 $ L.filter (\r -> L.or (L.map (\s -> s == _summary r) ss)) rs


















