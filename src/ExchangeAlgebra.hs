

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

    This umbrella exports named selections of the generic Algebra API,
    account titles, classifications and registry metadata, accounting exchange
    operations, transfers and closing, trial-balance balances and findings,
    financial-statement presentation, CSV and bookkeeping output, and value types.
    Start with this module for single-period bookkeeping. The transfer names
    '.->', '|%', 'Relabel' and 'transferEntries' belong to
    "ExchangeAlgebra.Accounting.Transfer"; import generic coordinate rules from
    "ExchangeAlgebra.Algebra.Transfer" qualified.

    > import ExchangeAlgebra
    >
    > -- A minimal exchange: cash increases by 100 (debit, 'decL') and
    > -- sales increase by 100 (credit, 'decR'). Build postings with '.@'.
    > entry :: Alg Double (HatBase AccountTitles)
    > entry = 100 .@ Not :< Cash .+ 100 .@ Not :< Sales

    == Choosing the value type (@v@ in @Alg v b@)

    * 'Prelude.Double' — fast IEEE-754; fine for simulations and quick work.
    * t'ExchangeAlgebra.Algebra.Value.MoneyDouble' — same speed, but a dedicated money
      newtype so a ledger value cannot be confused with a raw coefficient.
    * t'ExchangeAlgebra.Algebra.Value.MoneyDecimal' — exact decimal; construction-order
      independent totals. Use for audited\/deterministic ledgers.

    For multi-period simulation or metadata-aware journals (notes, axes),
    import "ExchangeAlgebra.Journal" qualified to avoid overlapping names
    such as @sigma@, @fromList@, @map@ and @filter@:

    > import           ExchangeAlgebra
    > import qualified ExchangeAlgebra.Journal as Journal
    >
    > journal :: Journal.Journal String Double (HatBase AccountTitles)
    > journal = (100 .@ Not :< Cash .+ 100 .@ Not :< Sales) Journal..| "sale"

    == Modules to import directly

    * "ExchangeAlgebra.Journal" — note-bearing postings and journal operations.
    * "ExchangeAlgebra.Simulation.Output" — @writeTermIO@ and @writeIOMatrix@.
    * "ExchangeAlgebra.Simulate.Spill" — spill writers, checked readers,
      @restoreJournalFromBinarySpill@ and @restoreJournalFromBinarySpillChecked@.
    * "ExchangeAlgebra.Accounting.Statements.Presentation" — detailed presentation
      configuration beyond t'ReportingContext', t'ReportingScope' and
      'jcciSecondGradeContext'.
    * "ExchangeAlgebra.Accounting.Statements.Group" and
      "ExchangeAlgebra.Accounting.Statements.Metric" — presentation groups,
      subtotal definitions and derived metrics.
    * "ExchangeAlgebra.Accounting.Entries" — adjusting and closing entries.
    * "ExchangeAlgebra.Accounting.Consolidation" — consolidation adjustments
      and provenance validation.
    * "ExchangeAlgebra.IO.Input" and "ExchangeAlgebra.IO.Input.Admission" —
      external input conversion and admission checks.
    * "ExchangeAlgebra.Simulate.Engine", "ExchangeAlgebra.Simulate.Lite",
      "ExchangeAlgebra.Simulate.Policy" and "ExchangeAlgebra.Simulation.Network" —
      simulation protocols, ledger policies and trade networks.
    * "ExchangeAlgebra.Simulation.Optimize" — optimization solvers.

    == Full examples

    Runnable examples (elementary bookkeeping and fixed-price simulations) live
    in the repository, not on Hackage:

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
    , unionsMerge
      -- * Accounts
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
    , projContra
      -- * Transfers and closing
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
    , settlementSteps
      -- * Trial balance and statements
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
    , present
      -- * Output
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
    , writeFinancialStatements
      -- * Values
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
