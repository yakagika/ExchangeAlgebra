
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns         #-} -- for initSTWorld
{-# LANGUAGE RecordWildCards        #-} -- for initSTWorld
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE StrictData             #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}


{-
状態空間による会計シミュレーションサンプル
4エージェントがこの取引を毎期繰り返すのみの尤も単純な形式
労働者等も存在しない.

-}


import qualified    Data.Text               as T
import qualified    Control.Monad                   as CM
import              Control.Monad
import              Control.Monad.ST
import              Data.Array.ST
import              Data.STRef
import qualified    Data.List                       as L


------------------------------------------------------------------
-- *  状態系の導入
------------------------------------------------------------------
class (Eq t, Show t, Ord t) => StateTime t where
    initTerm :: t
    lastTerm :: t
    nextTerm :: t -> t
    prevTerm :: t -> t


-- | 値の次の期の情報をどうするのかのパラメーター
data UpdatePattern = Copy         -- 前期の情報をそのままコピー
                   | Modify       -- 何らかの方法でupdate (単体でできる場合のみ)
                   | DoNothing    -- 放置
                   deriving (Show, Eq)

-- | 環境変数の系列
class StateVariables s k w a  where
    fromWorld :: (StateSpace s w) => w -> k -> a

    initialize      :: k -> ST s a

    updatePattern   :: k -> UpdatePattern

    copy            :: (StateSpace s w,StateTime t) => t -> w ->  ST s ()
    copy t w k = undefined

    modify          :: (StateSpace s w, StateTime t) => t -> w ->  ST s ()
    modify t w k = undefined

    update          :: (StateSpace s w, StateTime t) => t -> w -> ST s ()
    update t w k =  case updatePattern of
                        DoNothing -> return ()
                        Copy      -> copy   t w
                        Modify    -> modify t w

class  (Bounded k) => StateSpace s k w where
    initSS ::   (StateTime t) => t -> ST s w
    updateSS :: (StateTime t) => t -> w -> ST s ()
    updateSS t w = CM.forM_ ([minBound ... maxBound] :: k) $ \k -> update t w k

type Term = Int

instance StateTime Term where
    initTerm = 0
    lastTerm = 3
    nextTerm = \x -> x + 1
    prevTerm = \x -> x - 1

data VariableKinds = KindInt
                   | KindText
                   deriving (Show,Eq,Enum,Bounded)

type STI s = ST s (STRef s Int)
type STT s = ST s (STRef s Int)
data Variables s = I (STI s)
                 | T (STT s)

initI :: STI s
initI = newSTRef 0

initT :: STT s
initT = newSTRef "0"

instance StateVariables s VariableKinds (World s) (StateSpace s) where
    fromWorld wld KindInt = initI
    fromWorld wld KindText = initT


data World s = World {_int  :: Variables s
                     ,_text :: Variables s }


------------------------------------------------------------------
-- * 実行
------------------------------------------------------------------
main :: IO ()
main = print "xxxx"


