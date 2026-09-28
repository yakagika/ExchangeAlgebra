{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

-- | Input vocabulary at the IO input boundary. This package-private module
-- defines identifiers, registry rules, and the closed bookkeeping catalog for
-- the admission engine and public entry point. Read identifiers first, then
-- rules and submissions.
module ExchangeAlgebra.IO.Input.Admission.Types where

import Data.Hashable (Hashable(..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

import ExchangeAlgebra.Algebra (Alg)
import ExchangeAlgebra.Algebra.Base (AccountTitles, HatBase)
import ExchangeAlgebra.IO.Input.Checked (EntryError)
import ExchangeAlgebra.Journal (Note(..))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)

-- * Identifiers

-- | Exact entity identity; admission rejects blank identifiers.
newtype EntityId = EntityId Text deriving (Eq, Ord, Show)

-- | Exact reporting-period identity; no implicit current period is assumed.
newtype PeriodId = PeriodId Text deriving (Eq, Ord, Show)

-- | Transaction identity within one entity and period.
newtype TxId = TxId Text deriving (Eq, Ord, Show)

-- | Identity of a trusted entry in the specification's fact store.
newtype FactId = FactId Text deriving (Eq, Ord, Show)

-- | Identity of evidence stating a transaction's total debit amount.
newtype EvidenceId = EvidenceId Text deriving (Eq, Ord, Show)

-- | Invocation identity, independent of a generated transaction key.
newtype CallId = CallId Text deriving (Eq, Ord, Show)

-- | Exact transaction coordinate. Equality never performs wildcard matching.
data TxKey = TxKey EntityId PeriodId TxId -- ^ Entity, period, and transaction identity.
    deriving (Eq, Ord, Show)

-- | Hash the three exact text coordinates in their declared order.
instance Hashable TxKey where
    hashWithSalt salt (TxKey (EntityId entity) (PeriodId period) (TxId transaction)) =
        hashWithSalt salt (entity, period, transaction)

-- | Journal notes preserve transaction boundaries. The blank sentinel is
-- rejected by admission and is used only by the existing Journal interface.
instance Note TxKey where
    plank = TxKey (EntityId "") (PeriodId "") (TxId "")

-- | Concrete entry representation, retaining every original posting.
type Entry = Alg MoneyDecimal (HatBase AccountTitles)

-- | Untrusted side text, account text, and strictly positive monetary amount.
-- Parsing and balance checks are performed by @checkedEntryTextIn@.
type RawPostings = [(Text, Text, MoneyDecimal)]

-- * Registry and trusted specification

-- | Accounting role fixed by the trusted registry.
data Role
    = Ordinary -- ^ Ordinary journal transaction.
    | Opening -- ^ Opening balance transaction.
    | Adjustment -- ^ Adjustment before closing.
    | Closing -- ^ Closing transaction.
    | Elimination -- ^ Consolidation elimination.
    deriving (Eq, Ord, Show)

-- | Whether a transaction must occur in the admitted journal.
data Presence
    = Required -- ^ The transaction must occur.
    | Optional -- ^ The transaction may be absent.
    deriving (Eq, Show)

-- | One authorized supply route. Raw and fact roles are trusted declarations;
-- catalog roles and stages are determined solely by the operation kind.
data Supply
    = SupplyFacts FactId Role -- ^ Use a trusted fact and its declared role.
    | SupplySubmission Role -- ^ Use submitted postings with this role.
    | SupplyCatalog CatalogOpKind -- ^ Use the named catalog operation.
    deriving (Eq, Ord, Show)

-- | A registry rule. Its constructor and fields are package-private.
data TxRule = TxRule Presence (Set Supply) (Maybe EvidenceId) deriving (Eq, Show)

-- | Unique exact keys mapped to rules. Construct with @txidRegistry@.
newtype TxidRegistry = TxidRegistry (Map TxKey TxRule)

-- | Registry construction errors, detected before constructing the map.
data RegistryError
    = DuplicateRegistryKey TxKey -- ^ Repeated exact transaction key.
    | BlankRegistryKey TxKey -- ^ Blank key coordinate.
    | EmptySupplySet TxKey -- ^ No authorized supply route.
    | MixedFactSupply TxKey -- ^ Fact supply is not exclusive.
    | FactEvidenceForbidden TxKey -- ^ Facts cannot require external evidence.
    | MultipleSubmissionRoles TxKey -- ^ Conflicting submitted roles.
    | NonGeneratingSupply TxKey CatalogOpKind -- ^ Query used as a transaction supply.
    deriving (Eq, Show)

-- | Trusted admission contract. Evidence amounts are /total debit amounts
-- per transaction/, not net balances or individual posting amounts.
-- Facts are checked in the role declared by their registry entries.
data AdmissionSpec = AdmissionSpec
    { admissionRegistry   :: TxidRegistry
    , admissionEvidence   :: Map EvidenceId MoneyDecimal -- ^ Evidence debit totals.
    , admissionFacts      :: Map FactId RawPostings -- ^ Trusted raw fact entries.
    , admissionVocabulary :: Set AccountTitles -- ^ Allowed account titles.
    }

-- * Catalog inputs

-- | Closed operation identities used to authorize generated transactions.
data CatalogOpKind
    = CogsKind -- ^ Cost-of-goods adjustment.
    | DepIndirectKind -- ^ Indirect depreciation.
    | DepDirectKind -- ^ Direct depreciation.
    | AllowanceKind -- ^ Allowance replenishment.
    | AllowanceRateKind -- ^ Rate-based allowance.
    | AllowanceResetKind -- ^ Allowance reset.
    | PrepaidKind -- ^ Prepaid expense.
    | UnearnedKind -- ^ Unearned revenue.
    | AccruedRevenueKind -- ^ Accrued revenue.
    | AccruedExpenseKind -- ^ Accrued expense.
    | ReverseEntryKind -- ^ Reverse a source entry.
    | ConsumptionTaxKind -- ^ Consumption tax settlement.
    | CorporateInterimKind -- ^ Interim corporate tax.
    | CorporateSettlementKind -- ^ Corporate tax settlement.
    | EquityEarningsKind -- ^ Equity-method earnings.
    | EquityDividendKind -- ^ Equity-method dividend.
    | EquityEntriesKind -- ^ Combined equity-method entries.
    | EquityBalanceKind -- ^ Equity-method balance query.
    | PriorErrorKind -- ^ Prior-period error correction.
    | FinalStockKind -- ^ Final-stock closing transfer.
    | StraightLineKind -- ^ Straight-line depreciation.
    | ConsolidateKind -- ^ Consolidation balance check.
    deriving (Eq, Ord, Show)

-- | Unresolved company input. The entity label must match each key's entity.
-- Only the library constructs resolved inputs and their provenance.
data EntityInput = EntityInput EntityId (NonEmpty TxKey) -- ^ Entity and its source keys.
    deriving (Eq, Show)

-- | Closed bookkeeping requests. Amount parameters use the same currency
-- unit as raw postings; allowance rates use basis points (0 through 10000).
-- Account parameters and amounts are validated before any builder executes.
data CatalogCall
    = Cogs MoneyDecimal MoneyDecimal -- ^ Beginning and ending inventory amounts, in that order.
    | DepIndirect MoneyDecimal -- ^ Indirect depreciation amount.
    | DepDirect MoneyDecimal AccountTitles -- ^ Direct depreciation amount and asset account.
    | Allowance MoneyDecimal MoneyDecimal -- ^ Estimate and current allowance.
    | AllowanceRate MoneyDecimal -- ^ Rate in basis points.
    | AllowanceReset MoneyDecimal MoneyDecimal -- ^ Estimate and current allowance.
    | Prepaid MoneyDecimal AccountTitles -- ^ Amount and expense account.
    | Unearned MoneyDecimal AccountTitles -- ^ Amount and revenue account.
    | AccruedRevenueCall MoneyDecimal AccountTitles -- ^ Amount and revenue account.
    | AccruedExpenseCall MoneyDecimal AccountTitles -- ^ Amount and expense account.
    | ReverseEntry TxKey -- ^ Exact key of the entry to reverse.
    | ConsumptionTax MoneyDecimal MoneyDecimal -- ^ Paid and received amounts.
    | CorporateInterim MoneyDecimal -- ^ Interim tax amount.
    | CorporateSettlement MoneyDecimal MoneyDecimal -- ^ Total tax, then interim payment.
    | EquityEarnings MoneyDecimal -- ^ Share of earnings.
    | EquityDividend MoneyDecimal -- ^ Dividend amount.
    | EquityEntries MoneyDecimal MoneyDecimal -- ^ Earnings share and dividend.
    | EquityBalance -- ^ Query the equity-method balance.
    | PriorError -- ^ Current amount, prior amount, expense, then asset.
        MoneyDecimal MoneyDecimal AccountTitles AccountTitles
    | FinalStock -- ^ Close final stock against the visible ledger.
    | StraightLine -- ^ Asset, annual amount, then True for the direct method.
        AccountTitles MoneyDecimal Bool
    | Consolidate (NonEmpty EntityInput) (NonEmpty TxKey) -- ^ Entities and elimination keys.
    deriving (Eq, Show)

-- | An invocation in one entity and period. Posting operations require a
-- generated key in that scope; queries require 'Nothing'. A posting operation
-- that produces no rows still fulfills its declared generated key.
data Call = Call
    { callId        :: CallId
    , callEntity    :: EntityId -- ^ Entity scope of the call.
    , callPeriod    :: PeriodId -- ^ Period scope of the call.
    , callGenerated :: Maybe TxKey -- ^ Generated key, when applicable.
    , callBody      :: CatalogCall -- ^ Closed catalog request.
    }
    deriving (Eq, Show)

-- | Entire untrusted submission. Lists preserve duplicate keys for rejection.
-- The trusted executor must pass the complete submission to @admit@.
data Submission = Submission
    { submissionPostings :: [(TxKey, RawPostings)]
    , submissionCalls    :: [Call] -- ^ Submitted catalog calls.
    }
    deriving (Eq, Show)

-- * Diagnostics

-- | Library-controlled processing order, including the final query stage.
data Stage
    = OrdinaryStage -- ^ Opening and ordinary processing.
    | AdjustmentStage -- ^ Adjustment processing.
    | ConsolidationStage -- ^ Consolidation processing.
    | ClosingStage -- ^ Closing processing.
    | QueryStage -- ^ Read-only query processing.
    deriving (Eq, Ord, Show)

-- | Why a reference could not be resolved to an admitted source entry.
data ReferenceFailure
    = ReferenceUnknown -- ^ Key absent from the registry.
    | ReferenceAbsent -- ^ Registered key has no admitted entry.
    | ReferenceEntityMismatch EntityId -- ^ Reference belongs to another entity.
    | ReferencePeriodMismatch PeriodId -- ^ Reference belongs to another period.
    | ReferenceRoleMismatch Role -- ^ Source has a different accounting role.
    | ReferenceProvenanceForbidden Provenance -- ^ Source origin is not authorized.
    | ReferenceNotVisible Stage Stage -- ^ Source is outside the visible stage.
    | ReferenceReused -- ^ Reference was consumed more than once.
    deriving (Eq, Show)

-- | Structured failures collected by the fixed admission pipeline.
data AdmissionError
    = UnknownTransaction TxKey -- ^ Unregistered transaction.
    | SupplyNotAllowed TxKey (Maybe CatalogOpKind) (Set Supply) -- ^ Supply route is not authorized.
    | FactOverwrite TxKey FactId -- ^ Submitted key overwrites a fact.
    | MissingRequiredTransaction TxKey -- ^ Required transaction is absent.
    | DuplicateRawTransaction TxKey -- ^ Raw key occurs more than once.
    | DuplicateGeneratedTransaction TxKey -- ^ Generated key occurs more than once.
    | RawGeneratedCollision TxKey -- ^ Raw and generated keys collide.
    | DuplicateCallId CallId -- ^ Call identity occurs more than once.
    | InvalidCallIdentity CallId -- ^ Call identity or scope is blank.
    | MissingGeneratedKey CallId -- ^ Generating call lacks a key.
    | UnexpectedGeneratedKey CallId TxKey -- ^ Query has a generated key.
    | CallEntityMismatch CallId TxKey -- ^ Generated key has another entity.
    | CallPeriodMismatch CallId TxKey -- ^ Generated key has another period.
    | UnknownCallScope CallId EntityId PeriodId -- ^ Call scope is unregistered.
    | MissingFact TxKey FactId -- ^ Declared fact is absent.
    | MissingEvidence TxKey EvidenceId -- ^ Required evidence is absent.
    | InvalidEvidence TxKey EvidenceId MoneyDecimal -- ^ Evidence amount is not positive.
    | EvidenceMismatch TxKey EvidenceId MoneyDecimal MoneyDecimal -- ^ Evidence amount differs.
    | InvalidEntry TxKey (NonEmpty (EntryError MoneyDecimal)) -- ^ Entry conversion failed.
    | AccountOutsideVocabulary TxKey AccountTitles -- ^ Account is outside the vocabulary.
    | DirectPostingForbidden TxKey AccountTitles -- ^ Raw posting uses a protected account.
    | RawProfitEquityTransfer TxKey -- ^ Raw posting moves profit to equity.
    | GeneratedAccountForbidden CallId AccountTitles -- ^ Builder emitted an unauthorized account.
    | InvalidCatalogParameters CallId Text -- ^ Catalog parameters are invalid.
    | CatalogExecutionFailure CallId Text -- ^ Catalog builder failed.
    | StageRegression CallId Stage Stage -- ^ Calls regress in stage order.
    | UnresolvedReference CallId TxKey Role ReferenceFailure -- ^ Reference could not be admitted.
    | DuplicateEffect CallId TxKey -- ^ Generated posting duplicates a raw effect.
    deriving (Eq, Show)

-- | Origin established by admission, never accepted as a submitted label.
data Provenance
    = FactProvenance FactId -- ^ Originated from a trusted fact.
    | SubmissionProvenance (Maybe EvidenceId) -- ^ Originated from submitted postings.
    | CatalogProvenance CallId CatalogOpKind -- ^ Originated from a catalog call.
    deriving (Eq, Show)

-- | Observable audit record for a successfully executed invocation.
data CallAudit = CallAudit
    { auditCall       :: CallId
    , auditOperation  :: CatalogOpKind -- ^ Executed operation.
    , auditGenerated  :: Maybe TxKey -- ^ Declared generated key.
    , auditReferences :: [(TxKey, Provenance)] -- ^ Resolved keys and origins.
    , auditProjection :: Maybe MoneyDecimal -- ^ Query result, when applicable.
    }
    deriving (Eq, Show)
