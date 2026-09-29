-- | Compatibility entry point for the network API.
module ExchangeAlgebra.Simulate.Network
    {-# DEPRECATED "Use ExchangeAlgebra.Simulation.Network, ExchangeAlgebra.Simulation.Network.Flows, ExchangeAlgebra.Simulation.Network.Csv, and ExchangeAlgebra.Simulation.Network.Journal instead." #-}
    ( module ExchangeAlgebra.Simulation.Network
    , module ExchangeAlgebra.Simulation.Network.Flows
    , module ExchangeAlgebra.Simulation.Network.Csv
    , module ExchangeAlgebra.Simulation.Network.Journal
    ) where

import ExchangeAlgebra.Simulation.Network
import ExchangeAlgebra.Simulation.Network.Flows
import ExchangeAlgebra.Simulation.Network.Csv
import ExchangeAlgebra.Simulation.Network.Journal
