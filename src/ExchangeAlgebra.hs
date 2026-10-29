

{- |
    Module     : ExchangeAlgebra
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping systems.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    == Quick start

    This top-level module is the Algebra-layer umbrella: it re-exports
    "ExchangeAlgebra.Algebra", selected coordinate rules and closing operations,
    "ExchangeAlgebra.Accounting.Transfer",
    "ExchangeAlgebra.IO.Output.Statements", and "ExchangeAlgebra.Value". The transfer names
    '.->', '|%', 'Relabel' and 'transferEntries' are the accounting transfer of
    "ExchangeAlgebra.Accounting.Transfer"; import generic per-posting rules from
    "ExchangeAlgebra.Algebra.Transfer" qualified. It is the
    recommended entry point for simple single-period bookkeeping:

    > import ExchangeAlgebra
    >
    > -- A minimal exchange: cash increases by 100 (debit, 'decL') and
    > -- sales increase by 100 (credit, 'decR'). Build postings with '.@'.
    > entry :: Alg Double (HatBase AccountTitles)
    > entry = 100 .@ Not :< Cash .+ 100 .@ Not :< Sales

    == Choosing the value type (@v@ in @Alg v b@)

    * 'Prelude.Double' — fast IEEE-754; fine for simulations and quick work.
    * 'ExchangeAlgebra.Value.MoneyDouble' — same speed, but a dedicated money
      newtype so a ledger value cannot be confused with a raw coefficient.
    * t'ExchangeAlgebra.Value.MoneyDecimal' — exact decimal; construction-order
      independent totals. Use for audited\/deterministic ledgers.

    For multi-period simulation or metadata-aware journals (notes, axes),
    switch to the Journal layer. The two umbrellas export overlapping
    names (@sigma@, @fromList@, @map@, @filter@, …), so Journal-centric
    code should use qualified imports for the Algebra layer:

    > import           ExchangeAlgebra.Journal          -- umbrella + type classes
    > import qualified ExchangeAlgebra.Algebra          as EA
    > import qualified ExchangeAlgebra.Journal          as EJ
    > import qualified ExchangeAlgebra.Accounting.Transfer as Transfer
    > import qualified ExchangeAlgebra.Accounting.Closing  as Closing

    == Modules to import directly (not re-exported here)

    This umbrella intentionally re-exports only the Algebra-layer modules
    listed above. The following modules are __designed to be imported
    directly__ and are deliberately /not/ re-exported from @ExchangeAlgebra@,
    because they introduce names that would collide with the Algebra layer
    (or with each other) and are better used under a qualified import:

    * "ExchangeAlgebra.Accounting.Entries" — double-entry adjusting\/closing entries.
    * "ExchangeAlgebra.Accounting.Consolidation" — atomic consolidation
      adjustments, source provenance, and cross-statement linkage validation.
    * "ExchangeAlgebra.Accounting.TrialBalance.Validation" — trial-balance findings,
      reclassification instructions, and the policy-controlled reporting gate.
    * "ExchangeAlgebra.Accounting.Statements.Metric" — typed, read-only derived metrics
      that do not add account-basis coordinates.
    * "ExchangeAlgebra.Accounting.Statements.Group" — reusable gross, deduction, and net
      presentation groups for contra accounts.
    * "ExchangeAlgebra.Accounting.Statements.Presentation" — context-sensitive JGAAP
      transformation from validated trial balances to financial statements.
    * "ExchangeAlgebra.IO.Input.Admission" — accounting-integrity checks of
      untrusted journals and comparison with independent records.
    * "ExchangeAlgebra.Simulate" — the classic simulation engine. /(Removed
      from this umbrella in 0.5.0.0: it exports very generic names —
      @copy@, @modify@, @update@, @initialize@, @normal@, … — that polluted
      the bookkeeping namespace.)/
    * "ExchangeAlgebra.Simulate.Lite" — the lightweight simulation front-end.
    * "ExchangeAlgebra.Simulation.Network" — trade-network generators.
    * "ExchangeAlgebra.Simulate.Policy" — ledger\/spill policy configuration.
    * "ExchangeAlgebra.Simulation.Optimize" — pluggable optimization solvers
      ("ExchangeAlgebra.Simulation.Optimize.Annealing" \/ "ExchangeAlgebra.Simulation.Optimize.GA");
      generic names (@optimize@, @Config@, ...) best used qualified.

    Import them explicitly when you need them, e.g.:

    > import           ExchangeAlgebra
    > import qualified ExchangeAlgebra.Accounting.Entries     as BK
    > import           ExchangeAlgebra.Simulate          -- simulation engine
    > import qualified ExchangeAlgebra.Simulate.Lite   as Lite

    == Full examples

    Runnable examples (elementary bookkeeping, ripple-effect simulations,
    a CGE model) live in the repository, not on Hackage:

    <https://github.com/yakagika/ExchangeAlgebra/tree/master/examples>

    See the repository's @README.md@ for installation and consumption
    patterns.

-}


module ExchangeAlgebra
    ( -- * Algebra
      Hat(..)
    , HatBase(..)
    , HatBaseClass(..)
    , BaseClass(..)
    , BaseForSingleHat(..)
    , customError
    , Element(..)
    , AxisKey(..)
    , axisIsWildcard
    , AxisDecompose(..)
    , (.#)
    , Name
    , Subject
    , CountUnit(..)
    , Hashable(..)
    , Generic
    , Nearly(..)
    , isNearlyNum
    , Redundant(..)
    , HatVal(..)
    , ValueError(..)
    , isErrorValue
    , Rounding(..)
    , Tolerance(..)
    , defaultTolerance
    , sumValues
    , roundScaled
    , Alg(Zero, (:@), _val, _hatBase)
    , isZero
    , (.@)
    , (<@)
    , vals
    , bases
    , fromList
    , toList
    , foldEntries
    , sigma
    , sigma2When
    , sigmaFromMap
    , toASCList
    , map
    , mapPosting
    , mapMaybePosting
    , mapBasePart
    , extendBy
    , filter
    , proj
    , projNetNorm
    , balanceBy
    , balanceMapBy
    , netPairMapBy
    , foldEntriesToMap
    , decBy
    , postFromNetBy
      -- * Accounts
    , unionsMerge
    , AccountTitles(..)
    , AccountDivision(..)
    , Side(..)
    , ClosingRule(..)
    , FixedCurrent(..)
    , AccountRole(..)
    , PostingCapability(..)
    , DivisionSemantics(..)
    , HomeSideSemantics(..)
    , ReportingEligibility(..)
    , AccountSpec(..)
    , AccountSemantics(..)
    , accountAliases
    , accountSpec
    , accountSemantics
    , accountSpecMap
    , concreteAccountTitles
    , classifyAccountContra
    , accountDescriptions
    , PIMO(..)
    , switchSide
    , defaultSide
    , classifyAccountDivision
    , pimoFromDivision
    , pimoFlip
    , ExBaseClass(..)
    , AccountBase(..)
    , Exchange(..)
    , projCredit
    , projDebit
    , projByAccountTitle
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , projContraAssets
      -- * Transfers and closing
    , projContra
    , TransferScale(MulBy, DivBy)
    , TransferRule(..)
    , TransferRules
    , TransferRuleError(..)
    , TransferApplyError(..)
    , mkTransferRules
    , rulesToList
    , relabel
    , scaleBy
    , divideBy
    , collapseEntries
    , collapseNetEntries
    , TransferShare(..)
    , TransferEnds
    , TransferRow
    , TransferTable
    , (.->)
    , (|%)
    , mkTransferTable
    , tableRows
    , TransferTableError(..)
    , TransferError(..)
    , transferEntries
    , transferEntriesWith
    , TransferJournalError(..)
    , transferEntriesBy
    , transferEntriesByWith
    , ClosingSide(..)
    , closingSide
    , closingEntries
    , SettleRule
    , retainedEarningsRule
    , SettlementBatch
    , SignedNet
    , SettleError(..)
    , settleEntries
      -- * Trial balance and statements
    , settlementSteps
    , AccountBalance(..)
    , accountBalances
    , balanceFor
    , balanceSide
    , balanceAmount
    , TrialBalanceStage(..)
    , TrialBalanceInput(..)
    , ReclassificationRule(..)
    , TBFinding(..)
    , trialBalanceFindings
    , ReportingContext(..)
    , ReportingScope(..)
    , jcciSecondGradeContext
    , FinancialStatements(..)
    , StatementSection(..)
    , StatementLine(..)
    , PresentationIssue(..)
      -- * Output
    , present
    , writeCSV
    , csvTranspose
    , journalRows
    , writeJournal
    , accountLedgerRows
    , writeAccountOf
    , accountLedgerRowsJournal
    , writeAccountOfJournal
    , compoundTrialBalanceRows
    , writeCompoundTrialBalance
    , worksheetRows
    , writeWorksheet
    , postClosingTrialBalanceRows
    , writePostClosingTrialBalance
    , StatementRowsError(..)
    , financialStatementRows
      -- * Values
    , writeFinancialStatements
    , MoneyDecimal(..)
    , toDecimal
    , bankersRound
    , ceilingRound
    , MoneyParseError(..)
    , moneyDecimalFromText
    , moneyDecimalFromScientific
    , MoneyDouble(..)
    , toDouble
    , RationalAccum(..)
    , rationalAddAccum
    , rationalRoundAccum
    ) where


import Prelude ()
import ExchangeAlgebra.Algebra
import ExchangeAlgebra.Accounting.Account
    ( AccountTitles(..)
    , AccountDivision(..)
    , Side(..)
    , ClosingRule(..)
    , FixedCurrent(..)
    , AccountRole(..)
    , PostingCapability(..)
    , DivisionSemantics(..)
    , HomeSideSemantics(..)
    , ReportingEligibility(..)
    , AccountSpec(..)
    , AccountSemantics(..)
    , accountAliases
    , accountSpec
    , accountSemantics
    , accountSpecMap
    , concreteAccountTitles
    , classifyAccountContra
    , accountDescriptions
    , PIMO(..)
    , switchSide
    , defaultSide
    , classifyAccountDivision
    , pimoFromDivision
    , pimoFlip
    )
import ExchangeAlgebra.Accounting.Exchange
    ( ExBaseClass(..)
    , AccountBase(..)
    , Exchange(..)
    , projCredit
    , projDebit
    , projByAccountTitle
    , projCurrentAssets
    , projFixedAssets
    , projDeferredAssets
    , projCurrentLiability
    , projFixedLiability
    , projCapitalStock
    , projContraAssets
    , projContra
    )
-- Generic per-posting rules. The accounting transfer below owns Relabel and
-- transferEntries in this module, so they are not imported from here.
import ExchangeAlgebra.Algebra.Transfer.Representation
    ( TransferScale(MulBy, DivBy)
    , TransferRule(..)
    , TransferRules
    , TransferRuleError(..)
    , TransferApplyError(..)
    , mkTransferRules
    , rulesToList
    , relabel
    , scaleBy
    , divideBy
    , collapseEntries
    , collapseNetEntries
    )
import ExchangeAlgebra.Accounting.Transfer
import ExchangeAlgebra.Accounting.Closing
    ( ClosingSide(..)
    , closingSide
    , closingEntries
    , SettleRule
    , retainedEarningsRule
    , SettlementBatch
    , SignedNet
    , SettleError(..)
    , settleEntries
    , settlementSteps
    )
import ExchangeAlgebra.Accounting.TrialBalance.Balance
    ( AccountBalance(..)
    , accountBalances
    , balanceFor
    , balanceSide
    , balanceAmount
    )
import ExchangeAlgebra.Accounting.TrialBalance.Validation
    ( TrialBalanceStage(..)
    , TrialBalanceInput(..)
    , ReclassificationRule(..)
    , TBFinding(..)
    , trialBalanceFindings
    )
import ExchangeAlgebra.Accounting.Statements.Presentation
    ( ReportingContext(..)
    , ReportingScope(..)
    , jcciSecondGradeContext
    , FinancialStatements(..)
    , StatementSection(..)
    , StatementLine(..)
    , PresentationIssue(..)
    , present
    )
import ExchangeAlgebra.IO.Output.Csv (writeCSV, csvTranspose)
import ExchangeAlgebra.IO.Output.Statements
    ( journalRows
    , writeJournal
    , accountLedgerRows
    , writeAccountOf
    , accountLedgerRowsJournal
    , writeAccountOfJournal
    , compoundTrialBalanceRows
    , writeCompoundTrialBalance
    , worksheetRows
    , writeWorksheet
    , postClosingTrialBalanceRows
    , writePostClosingTrialBalance
    , StatementRowsError(..)
    , financialStatementRows
    , writeFinancialStatements
    )
import ExchangeAlgebra.Algebra.Value
    ( MoneyDecimal(..)
    , toDecimal
    , bankersRound
    , ceilingRound
    , MoneyParseError(..)
    , moneyDecimalFromText
    , moneyDecimalFromScientific
    , MoneyDouble(..)
    , toDouble
    , RationalAccum(..)
    , rationalAddAccum
    , rationalRoundAccum
    )
