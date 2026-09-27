{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Construct exact transaction registries in the IO input layer. The public
-- admission API uses this module to check keys before constructing a map, so
-- duplicate rules cannot disappear. Read the rule getters before 'txidRegistry'.
module ExchangeAlgebra.IO.Input.Admission.Registry
    ( -- * Rule construction and observations
      txRule
    , rulePresence
    , ruleSupplies
    , ruleEvidence
      -- * Registry construction and observations
    , registryRules
    , txidRegistry
      -- * Duplicate and key checks
    , duplicates
    , isBlankKey
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

import ExchangeAlgebra.IO.Input.Admission.Catalog (isGenerating)
import ExchangeAlgebra.IO.Input.Admission.Types

-- | Declare presence, authorized routes, and an independent debit-total evidence
-- obligation. Registry construction rejects empty or conflicting route sets.
txRule :: Presence -> [Supply] -> Maybe EvidenceId -> TxRule
txRule presence supplies = TxRule presence (Set.fromList supplies)

-- | Read the transaction's presence obligation.
rulePresence :: TxRule -> Presence
rulePresence (TxRule presence _ _) = presence

-- | Read all authorized routes without selecting the actual source.
ruleSupplies :: TxRule -> Set Supply
ruleSupplies (TxRule _ supplies _) = supplies

-- | Read the evidence obligation shared by every authorized supply route.
ruleEvidence :: TxRule -> Maybe EvidenceId
ruleEvidence (TxRule _ _ evidence) = evidence

-- | Return a copy of the exact key-to-rule mapping.
registryRules :: TxidRegistry -> Map TxKey TxRule
registryRules (TxidRegistry rules) = rules

-- | Find repeated values without discarding any input before counting it.
duplicates :: Ord a => [a] -> [a]
duplicates = Map.keys . Map.filter (> (1 :: Int)) . Map.fromListWith (+)
    . map (\value -> (value, 1))

-- | Reject empty or whitespace-only entity, period, or transaction identities.
isBlankKey :: TxKey -> Bool
isBlankKey (TxKey (EntityId entity) (PeriodId period) (TxId transaction)) =
    any (Text.null . Text.strip) [entity, period, transaction]

-- | Construct a registry after checking every duplicate, blank key, and
-- conflicting supply routes. Query-only operations cannot supply transactions.
--
-- Laws: for finite rules with unique nonblank keys and valid supply sets,
-- successful construction preserves every rule under 'registryRules'. For
-- any repeated key, construction fails irrespective of row order. Observation
-- is exact map equality; no monetary tolerance or type-class instance applies.
txidRegistry :: [(TxKey, TxRule)] -> Either (NonEmpty RegistryError) TxidRegistry
txidRegistry rows = case errors of
    first : rest -> Left (first :| rest)
    [] -> Right (TxidRegistry (Map.fromList rows))
  where
    errors = map DuplicateRegistryKey (duplicates (map fst rows))
        ++ [BlankRegistryKey key | (key, _) <- rows, isBlankKey key]
        ++ concatMap supplyErrors rows
    supplyErrors (key, rule) =
        [EmptySupplySet key | Set.null supplies]
        ++ [MixedFactSupply key | not (null facts), Set.size supplies /= 1]
        ++ [FactEvidenceForbidden key | not (null facts), ruleEvidence rule /= Nothing]
        ++ [MultipleSubmissionRoles key | length rawRoles > 1]
        ++ [NonGeneratingSupply key kind
           | SupplyCatalog kind <- Set.toList supplies, not (isGenerating kind)]
      where
        supplies = ruleSupplies rule
        facts = [fact | SupplyFacts fact _ <- Set.toList supplies]
        rawRoles = [role | SupplySubmission role <- Set.toList supplies]
