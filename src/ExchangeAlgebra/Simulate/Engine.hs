{- |
    Module     : ExchangeAlgebra.Simulate.Engine
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    The state-space engine of "ExchangeAlgebra.Simulate" on its own: terms
    ('StateTime'), the per-variable update protocol ('Updatable',
    'UpdatePattern', the @ST@ wrappers 'UpdatableSTRef' \/ 'UpdatableSTArray'),
    events ('Event', 'eventAll'), the world type class 'StateSpace', and the
    runners ('runSimulation', 'runScenarios' and their spill variants).

    Every name is re-exported unchanged from "ExchangeAlgebra.Simulate", with
    the same export restrictions ('Updatable' exposes @unwrap@ and @Inner@,
    'StateSpace' exposes @event@ \/ @randomSeeds@ \/ @initT@ \/ @lastT@). The
    spill configuration and readers ('ExchangeAlgebra.Simulate.Spill.SpillOptions'
    and friends) are deliberately not re-exported here: import
    "ExchangeAlgebra.Simulate.Spill" for them. The ripple-effect utilities live
    in "ExchangeAlgebra.Simulate.Analysis" and the random helpers in
    "ExchangeAlgebra.Simulate.Random".
-}

module ExchangeAlgebra.Simulate.Engine
    ( -- * Terms
      StateTime
    , initTerm
    , lastTerm
    , nextTerm
    , prevTerm
      -- * Per-variable update protocol
    , UpdatePattern(..)
    , Updatable(unwrap, Inner)
    , initialize
    , updatePattern
    , copy
    , modify
    , update
    , InitVariables
    , UpdatableSTRef(..)
    , UpdatableSTArray(..)
    , modifyArray
      -- * Events
    , Event(..)
    , eventAll
      -- * World state
    , StateSpace(event, randomSeeds, initT, lastT)
    , initAll
    , updateAll
      -- * Runners
    , runSimulation
    , runSimulationWithSpill
    , runScenarios
    , runScenariosWithSpill
    ) where

import           ExchangeAlgebra.Simulate
                     ( StateTime
                     , initTerm
                     , lastTerm
                     , nextTerm
                     , prevTerm
                     , UpdatePattern(..)
                     , Updatable(unwrap, Inner)
                     , initialize
                     , updatePattern
                     , copy
                     , modify
                     , update
                     , InitVariables
                     , UpdatableSTRef(..)
                     , UpdatableSTArray(..)
                     , modifyArray
                     , Event(..)
                     , eventAll
                     , StateSpace(event, randomSeeds, initT, lastT)
                     , initAll
                     , updateAll
                     , runSimulation
                     , runSimulationWithSpill
                     , runScenarios
                     , runScenariosWithSpill
                     )
