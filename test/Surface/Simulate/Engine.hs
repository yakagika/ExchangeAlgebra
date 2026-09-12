{-# LANGUAGE TypeFamilies #-}

-- | Compile-time lock for the public names exported by
-- "ExchangeAlgebra.Simulate.Engine".
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
    ) where

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

