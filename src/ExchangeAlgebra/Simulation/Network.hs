{- | Construct and inspect trade networks in the Simulation layer.
The private representation stores nodes, directed edges, and sparse input
coefficients. Generators and accessors are available here; flow generation,
CSV input, and Journal summation live in separate adapters.

An t'InputCoefficients' value checks that coefficients are non-negative, finite,
and attached to network edges. It does not establish matrix invertibility.
Read a network with 'nodes' and its coefficients with 'coefficient' or
'inputsOf'.

@ 
Task                           Import
Build and inspect networks     ExchangeAlgebra.Simulation.Network
Generate industrial flows      ExchangeAlgebra.Simulation.Network.Flows
Read network CSV files         ExchangeAlgebra.Simulation.Network.Csv
Sum into a Journal             ExchangeAlgebra.Simulation.Network.Journal
@

== What this module is

A small, additive front-end that separates two concepts that the older
examples conflated into a single dense @N×N@ coefficient matrix:

  1. the __trade network__, t'TradeNetwork' — /who may trade with whom/, a
     sparse directed relation; and
  2. the __input coefficients__, t'InputCoefficients' — /the technology/,
     a sparse map of per-edge coefficients @a_{ij}@.

In the dense-matrix style the support (non-zero cells) of the coefficient
matrix /was/ the trade relation, so sparsity was an accident of the data
representation rather than a modeling choice. Splitting them lets a model
pick its market structure (complete, @k@-regular, Erdős–Rényi, scale-free,
sectoral) independently of the coefficients, and lets the summation

@
'ExchangeAlgebra.Simulation.Network.Journal.sigmaEdges' g f
@

run the familiar \"Σ\" notation over the /edges/ of @g@ (cost @O(E)@) instead
of over all ordered pairs (cost @O(N²)@). With 'completeNetwork' the two
coincide, so an existing all-pairs model can be ported without changing the
notation (see "ExchangeAlgebra.Simulation.Network.Journal").

'industrialNetwork' builds an ordered block-triangular, power-law trade graph;
"ExchangeAlgebra.Simulation.Network.Flows" computes its exact-integer,
demand-driven backward substitution.

== Edge orientation

An edge @(i, j)@ means \"@i@ is a /supplier/ of @j@\" (equivalently \"@j@ is a
/buyer/ from @i@\"). The coefficient @a_{ij}@ attached to that edge is \"the
amount of @i@ that one unit of @j@'s output requires\". This matches the
long-form table layout @(from, to, coef)@ and the @(supplier, buyer)@ index
order of the example input-coefficient tables.

== Determinism

Every generator is a pure function of an explicit 'StdGen' (it does not
return a generator; split one yourself with 'System.Random.split' if you
need an independent stream). The same seed always yields the same network,
and all read-outs ('nodes', 'edges', 'suppliersOf', 'buyersOf', 'inputsOf')
return their results in ascending 'Ord' order, never in hash-table order.

== Internal representation is private

t'TradeNetwork', t'InputCoefficients' and 'NetworkError' are abstract: their
constructors are not exported, so the invariants (out\/in adjacency agree,
@supp(A) ⊆ edges(G)@, no self-loops, non-negative coefficients) cannot be
broken from outside. Build values with the smart constructors and the
generators; read them with the accessors.

== Using a network with the classic "ExchangeAlgebra.Simulate"

This module deliberately provides /no/ @Updatable@ instance for the network
types (the @Updatable t v a s | a s -> t v@ functional dependency makes it
impossible for the library to fix the user's @(t, v)@). To carry a (read-only)
network in a classic simulation, wrap it in your own @UpdatableSTRef@ cell:

@
newtype NetCell s = NetCell (Data.STRef.STRef s (TradeNetwork Int))
instance UpdatableSTRef NetCell s (TradeNetwork Int)
@

and read it inside an event with @readURef@. In the newer
"ExchangeAlgebra.Simulate.Lite" front-end the network is simply a @carry@
field (it never changes during a run), with no instance at all.
-}
module ExchangeAlgebra.Simulation.Network
    ( TradeNetwork, InputCoefficients, NetworkError(..)
    , tradeNetwork, inputCoefficients
    , nodes, edges, suppliersOf, buyersOf, edgeCount, coefficient, inputsOf
    , completeNetwork, circulant, kRegular, erdosRenyi, scaleFree, sectorBlock
    , IndustrialEconomy(..), IndustrialOptions(..), defaultIndustrialOptions
    , industrialNetwork, industrialNetworkWith, firms, industrialEdges
    , CoefOptions(..), defaultCoefOptions, randomCoefficients
    , networkFromTable, coefficientsFromTable, fromCoefficientMatrix
    ) where

import ExchangeAlgebra.Simulation.Network.Representation
