{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Compile-time lock for the public names exported by
-- "ExchangeAlgebra.Simulate.Engine" and the observer runner signatures from
-- "ExchangeAlgebra.Simulate.Lite".
module Surface.Simulate.Engine
    ( StateTime
    , initTerm
    , lastTerm
    , nextTerm
    , prevTerm
    , UpdatePattern (Copy, Modify, DoNothing)
    , Updatable (unwrap, Inner)
    , initialize
    , updatePattern
    , copy
    , modify
    , update
    , InitVariables
    , UpdatableSTRef
          ( _unwrapURef
          , _wrapURef
          , newURef
          , readURef
          , writeURef
          , modifyURef
          )
    , UpdatableSTArray
          ( _unwrapUArray
          , _wrapUArray
          , getUBounds
          , newUArray
          , readUArray
          , writeUArray
          , modifyUArray
          )
    , modifyArray
    , Event (fstEvent, lastEvent)
    , eventAll
    , StateSpace (event, randomSeeds, initT, lastT)
    , initAll
    , updateAll
    , runSimulation
    , runSimulationWithSpill
    , runScenarios
    , runScenariosWithSpill
    , runLiteFold
    , runLiteWithPolicyObs
    ) where

import qualified Data.Binary                     as Binary

import           ExchangeAlgebra.Journal          ( HatBaseClass
                                                   , HatVal
                                                   , Journal
                                                   , Note
                                                   )
import           ExchangeAlgebra.Simulate.Engine
                     ( StateTime
                     , initTerm
                     , lastTerm
                     , nextTerm
                     , prevTerm
                     , UpdatePattern (Copy, Modify, DoNothing)
                     , Updatable (unwrap, Inner)
                     , initialize
                     , updatePattern
                     , copy
                     , modify
                     , update
                     , InitVariables
                     , UpdatableSTRef
                           ( _unwrapURef
                           , _wrapURef
                           , newURef
                           , readURef
                           , writeURef
                           , modifyURef
                           )
                     , UpdatableSTArray
                           ( _unwrapUArray
                           , _wrapUArray
                           , getUBounds
                           , newUArray
                           , readUArray
                           , writeUArray
                           , modifyUArray
                           )
                     , modifyArray
                     , Event (fstEvent, lastEvent)
                     , eventAll
                     , StateSpace (event, randomSeeds, initT, lastT)
                     , initAll
                     , updateAll
                     , runSimulation
                     , runSimulationWithSpill
                     , runScenarios
                     , runScenariosWithSpill
                     )
import           ExchangeAlgebra.Simulate.Lite    ( InitT
                                                   , LiteWorld
                                                   , SimSpec
                                                   , SnapT
                                                   )
import qualified ExchangeAlgebra.Simulate.Lite   as Lite
import           ExchangeAlgebra.Simulate.Policy  ( HasTermAxis
                                                   , LedgerPolicy
                                                   , TermOf
                                                   )

-- | Compile-time lock for the pure per-term observer runner exported by
-- "ExchangeAlgebra.Simulate.Lite".
runLiteFold
    :: forall w t n v b acc r.
       ( forall s. LiteWorld w s
       , HatVal v, HatBaseClass b, Note n, Enum t, Ord t )
    => (t -> w SnapT -> acc -> acc)
    -> acc
    -> SimSpec w t n v b
    -> w InitT
    -> (acc -> w SnapT -> r)
    -> r
runLiteFold = Lite.runLiteFold

-- | Compile-time lock for the policy runner with a per-term read-only
-- observer exported by "ExchangeAlgebra.Simulate.Lite".
runLiteWithPolicyObs
    :: forall w t n v b r.
       ( forall s. LiteWorld w s
       , HatVal v, HatBaseClass b
       , HasTermAxis n, TermOf n ~ t
       , StateTime t
       , Binary.Binary t, Binary.Binary (Journal n v b) )
    => (t -> w SnapT -> IO ())
    -> LedgerPolicy
    -> SimSpec w t n v b
    -> w InitT
    -> (w SnapT -> r)
    -> IO r
runLiteWithPolicyObs = Lite.runLiteWithPolicyObs
