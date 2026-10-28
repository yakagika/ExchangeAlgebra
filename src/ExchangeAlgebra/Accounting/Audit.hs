{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Accounting.Audit
Description : One function per type of accounting anomaly, reported without blocking.

Role: each function detects one type of accounting anomaly that can be judged
without knowing the correct answer, and returns it as a value. Computation
('ExchangeAlgebra.IO.Input.Admission.admit',
'ExchangeAlgebra.Accounting.Statements.Presentation.present') never consults
these audits; the caller, a person or a language model, decides whether to
stop, correct, or accept each finding.

Layer: Accounting. The audits read account semantics (home side, role,
statement division) from the account registry. Callers are model authors,
teaching material, and experiments that check model output.

Every audit returns an 'AuditResult'. Whether a function reports 'Warning' or
'Error' is fixed per function by the table below. Invalid values and wildcard
bases are checked first by every function: if any is found, the function
returns 'Error' with 'StructureFinding' and inspects nothing else. 'Warning'
holds accounting findings only.

+----------------------------+-------------------------------+--------------------------+---------+
| Function                   | Reports                       | Precondition             | Result  |
+============================+===============================+==========================+=========+
| 'impossibleBalances'       | Credit balance of cash or     | Trial balance including  | Error   |
|                            | petty cash, debit balance of  | the opening balances     |         |
|                            | capital stock or accumulated  |                          |         |
|                            | depreciation                  |                          |         |
+----------------------------+-------------------------------+--------------------------+---------+
| 'abnormalBalances'         | Balance opposite to a fixed   | Trial balance including  | Warning |
|                            | home side, except the         | the opening balances     |         |
|                            | accounts above                |                          |         |
+----------------------------+-------------------------------+--------------------------+---------+
| 'temporaryAccountBalances' | Balance of suspense payments, | Trial balance after      | Warning |
|                            | suspense receipts, or the     | adjustment               |         |
|                            | suspense account              |                          |         |
+----------------------------+-------------------------------+--------------------------+---------+
| 'closingResiduals'         | Balance that closing must     | Trial balance after      | Error   |
|                            | clear                         | closing                  |         |
+----------------------------+-------------------------------+--------------------------+---------+
| 'duplicateTransactions'    | Notes with identical postings | Journal                  | Warning |
+----------------------------+-------------------------------+--------------------------+---------+
| 'contraryNominalPostings'  | Revenue or expense posting    | Journal, one transaction | Warning |
|                            | opposite to its home side,    | per note                 |         |
|                            | outside closing               |                          |         |
+----------------------------+-------------------------------+--------------------------+---------+

The trial-balance audits read balances as
'ExchangeAlgebra.Accounting.TrialBalance.Balance.accountBalances' does: the
exact posting sum of each account, its direction determined before rounding
the magnitude once, without a tolerance. A balance that nets to exactly zero is
no balance. The input may hold unaggregated postings; the caller does not need
to apply @bar@ first, and the audits do not aggregate the caller's value.

Preconditions are stated by the function name and its documentation; the
argument types do not enforce them. A trial-balance audit given a partial
period reports balances of that partial period.

Structure issues are ordered by kind (invalid value, wildcard account, wildcard
side), then base and value, with journal issues ordered first by note, while
trial-balance accounting findings are ordered by account.

Read 'AuditResult' and 'Finding' first, then the trial-balance audits, then the
journal audits.
-}
module ExchangeAlgebra.Accounting.Audit
    ( -- * Results
      AuditResult(..)
    , Finding(..)
    , StructureIssue(..)
    , TrialBalanceIssue
    , JournalIssue
      -- * Trial-balance audits
    , ImpossibleBalance(..)
    , impossibleBalances
    , AbnormalBalance(..)
    , abnormalBalances
    , TemporaryAccountBalance(..)
    , temporaryAccountBalances
    , ClosingResidual(..)
    , closingResiduals
      -- * Journal audits
    , DuplicateTransactions(..)
    , duplicateTransactions
    , ContraryNominalPosting(..)
    , contraryNominalPostings
    ) where

import qualified Data.HashMap.Strict as HM
import           Data.List (sort, sortOn)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M

import           ExchangeAlgebra.Accounting.Account.Classification
                     ( AccountDivision(..)
                     , AccountRole(..)
                     , DivisionSemantics(..)
                     , HomeSideSemantics(..)
                     , PostingCapability(..)
                     , Side(..)
                     )
import           ExchangeAlgebra.Accounting.Account.PostingPolicy (postingCapabilityFor)
import           ExchangeAlgebra.Accounting.Account.Registry
                     (AccountSemantics(..), accountSemantics)
import           ExchangeAlgebra.Accounting.Account.Title (AccountTitles(..))
import           ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import           ExchangeAlgebra.Accounting.TrialBalance.Balance
                     ( AccountBalance(..)
                     , TrialBalance
                     , accountBalances
                     , balanceSide
                     )
import           ExchangeAlgebra.Algebra.Base.Representation (Hat(..), HatBase, HatBaseClass(hat))
import           ExchangeAlgebra.Algebra.Core.Representation (Alg, pattern (:@), foldEntries)
import           ExchangeAlgebra.Algebra.Value.Class (HatVal, isErrorValue)
import           ExchangeAlgebra.Journal.Core (Journal, Note(..), toMap)

-- * Results

-- | The outcome of one audit, with structure issues @s@ and accounting
-- findings @f@. A function returns either 'Warning' or 'Error' for accounting
-- findings, fixed by the table in the module header. Structure issues appear
-- only in 'Error'.
--
-- 'Warning' marks a state with legitimate exceptions: it may be a mistake or
-- the result of correct processing. 'Error' marks a state with no legitimate
-- exception: some posting or computation is wrong, or the input cannot be
-- audited.
data AuditResult s f
    = NoProblem                     -- ^ The audit found nothing.
    | Warning (NonEmpty f)          -- ^ Findings that have legitimate exceptions.
    | Error (NonEmpty (Finding s f)) -- ^ Findings without a legitimate exception,
                                    -- or structure issues.
    deriving (Show, Eq)

-- | An element of 'Error': a structure issue that prevents the audit, or an
-- accounting finding of the audit's own type.
data Finding s f
    = StructureFinding s     -- ^ The input cannot be audited.
    | AccountingFinding f    -- ^ The anomaly this audit looks for.
    deriving (Show, Eq)

-- | Elements that no audit can read. These are the same checks
-- 'ExchangeAlgebra.Accounting.Statements.Presentation.present' applies
-- before presenting statements.
data StructureIssue v b
    = InvalidValue v b       -- ^ A value that is not a valid posting value.
    | WildcardAccount b      -- ^ A base whose account title is the wildcard.
    | WildcardSide b         -- ^ A base whose side is the wildcard @HatNot@.
    deriving (Show, Eq)

-- | Structure issues of a trial balance.
type TrialBalanceIssue v = StructureIssue v (HatBase AccountTitles)

-- | Structure issues of a journal, with the note that holds the element.
type JournalIssue n v b = (n, StructureIssue v b)

-- | Check every posting before any operation that determines accounting sides.
-- A standalone zero term is inspected just as in the presentation structure check.
structureIssues
    :: forall v b. (HatVal v, ExBaseClass b)
    => Alg v b
    -> [StructureIssue v b]
structureIssues input =
    [InvalidValue value base | (base, value) <- postings, isErrorValue value]
    ++ [WildcardAccount base | (base, _) <- postings, getAccountTitle base == AccountTitle]
    ++ [WildcardSide base | (base, _) <- postings, hat base == HatNot]
  where
    postings :: [(b, v)]
    postings = sort $ case input of
        value :@ base -> [(base, value)]
        _             -> foldEntries (\entries value base -> (base, value) : entries) [] input

-- | Specialize the common structure check to account-only trial balances.
trialBalanceStructureIssues :: HatVal v => TrialBalance v -> [TrialBalanceIssue v]
trialBalanceStructureIssues = structureIssues

-- | Attach notes to structure issues, including those of the blank note.
journalStructureIssues
    :: (HatVal v, ExBaseClass b, Note n)
    => Journal n v b
    -> [JournalIssue n v b]
journalStructureIssues input =
    [ (note, issue)
    | (note, transaction) <- sortOn fst (HM.toList (toMap input))
    , issue <- structureIssues transaction
    ]

-- | Return warning findings only when the list is nonempty.
warningFindings :: [f] -> AuditResult s f
warningFindings []                   = NoProblem
warningFindings (finding : findings) = Warning (finding :| findings)

-- | Return accounting errors only when the list is nonempty.
errorFindings :: [f] -> AuditResult s f
errorFindings []                   = NoProblem
errorFindings (finding : findings) =
    Error (AccountingFinding finding :| map AccountingFinding findings)

-- | Accounts and the balance directions that cannot occur in correct processing.
impossibleBalanceSides :: [(AccountTitles, Side)]
impossibleBalanceSides =
    [ (Cash, Credit)
    , (PettyCash, Credit)
    , (CapitalStock, Debit)
    , (AccumulatedDepreciation, Debit)
    ]

-- * Trial-balance audits

-- | An account whose balance cannot occur under any correct processing.
data ImpossibleBalance v = ImpossibleBalance
    { _impossibleAccount :: AccountTitles     -- ^ The account.
    , _impossibleBalance :: AccountBalance v  -- ^ Its balance.
    }
    deriving (Show, Eq)

-- | Report credit balances of cash and petty cash, and debit balances of
-- capital stock and accumulated depreciation. Returns 'Error' when any is
-- found.
--
-- Precondition: the trial balance includes the opening balances, so that a
-- balance is the balance of the account and not of a partial period.
--
-- Cash on hand is a counted quantity and cannot be negative; a bank overdraft
-- is a liability, not cash (Kieso, Intermediate Accounting, 2025, p. 6-4).
-- Capital stock is credited on issuance and debited only on retirement of
-- issued shares (p. 14-8). Accumulated depreciation is debited only when a
-- disposal removes the amount already recorded (p. 10-26). A credit balance of
-- current deposits is not reported here; see 'abnormalBalances'.
impossibleBalances
    :: HatVal v
    => TrialBalance v
    -> AuditResult (TrialBalanceIssue v) (ImpossibleBalance v)
impossibleBalances input = case trialBalanceStructureIssues input of
    issue : issues -> Error (StructureFinding issue :| map StructureFinding issues)
    []             -> errorFindings
        [ ImpossibleBalance title balance
        | (title, balance) <- M.toAscList (accountBalances input)
        , Just (balanceSide balance) == lookup title impossibleBalanceSides
        ]

-- | An account balance on the side opposite to its home side.
data AbnormalBalance v = AbnormalBalance
    { _abnormalAccount  :: AccountTitles     -- ^ The account.
    , _abnormalHomeSide :: Side              -- ^ The side its balance normally has.
    , _abnormalBalance  :: AccountBalance v  -- ^ Its balance.
    }
    deriving (Show, Eq)

-- | Report balances on the side opposite to the home side, for ordinary
-- accounts whose home side is fixed. The accounts of 'impossibleBalances' are
-- excluded. Returns 'Warning' when any is found.
--
-- Precondition: the trial balance includes the opening balances.
--
-- Legitimate exceptions: a deficit (debit balance of retained earnings), a
-- credit balance of current deposits before it is reclassified to an
-- overdraft at closing
-- ('ExchangeAlgebra.Accounting.Adjustment.overdraftReclassification'), a debit
-- balance of the allowance for doubtful accounts during the period, a credit
-- balance of an expense after a reversing entry, a credit balance of accounts
-- receivable (advance received), and a debit balance of accounts payable
-- (advance paid).
abnormalBalances
    :: HatVal v
    => TrialBalance v
    -> AuditResult (TrialBalanceIssue v) (AbnormalBalance v)
abnormalBalances input = case trialBalanceStructureIssues input of
    issue : issues -> Error (StructureFinding issue :| map StructureFinding issues)
    []             -> warningFindings
        [ AbnormalBalance title side balance
        | (title, balance) <- M.toAscList (accountBalances input)
        , title `notElem` map fst impossibleBalanceSides
        , balance /= NoBalance
        , Just semantics <- [accountSemantics title]
        , OrdinaryAccount `elem` asemRoles semantics
        , FixedHomeSide side <- [asemHomeSideSemantics semantics]
        , balanceSide balance /= side
        ]

-- | A temporary account that still has a balance.
data TemporaryAccountBalance v = TemporaryAccountBalance
    { _temporaryAccount :: AccountTitles     -- ^ The account.
    , _temporaryBalance :: AccountBalance v  -- ^ Its balance.
    }
    deriving (Show, Eq)

-- | Report balances of suspense payments, suspense receipts, and the suspense
-- account. Cash over and short is reported by 'closingResiduals'. Returns
-- 'Warning' when any is found.
--
-- Precondition: the trial balance is taken after adjustment.
--
-- Legitimate exception: an item still under investigation. A suspense payment
-- or receipt is transferred when its content is settled; there is no general
-- rule that it must be cleared at the period end.
temporaryAccountBalances
    :: HatVal v
    => TrialBalance v
    -> AuditResult (TrialBalanceIssue v) (TemporaryAccountBalance v)
temporaryAccountBalances input = case trialBalanceStructureIssues input of
    issue : issues -> Error (StructureFinding issue :| map StructureFinding issues)
    []             -> warningFindings
        [ TemporaryAccountBalance title balance
        | (title, balance) <- M.toAscList (accountBalances input)
        , title `elem` [SuspensePayments, SuspenseReceipts, SuspenseAccount]
        , balance /= NoBalance
        ]

-- | An account that closing must clear but that still has a balance.
data ClosingResidual v = ClosingResidual
    { _residualAccount :: AccountTitles     -- ^ The account.
    , _residualBalance :: AccountBalance v  -- ^ Its balance.
    }
    deriving (Show, Eq)

-- | Report balances that closing must clear: income summary, cash over and
-- short, period results and reporting subtotals (net income and others), and
-- accounts in the revenue and expense divisions. Returns 'Error' when any is
-- found.
--
-- Precondition: the trial balance is taken after closing.
closingResiduals
    :: HatVal v
    => TrialBalance v
    -> AuditResult (TrialBalanceIssue v) (ClosingResidual v)
closingResiduals input = case trialBalanceStructureIssues input of
    issue : issues -> Error (StructureFinding issue :| map StructureFinding issues)
    []             -> errorFindings
        [ ClosingResidual title balance
        | (title, balance) <- M.toAscList (accountBalances input)
        , balance /= NoBalance
        , Just semantics <- [accountSemantics title]
        , title == CashOverShort
            || any (`elem` asemRoles semantics) [ClosingDevice, PeriodResult, ReportingSubtotal]
            || asemDivisionSemantics semantics `elem`
                [StatementDivision Revenue, StatementDivision Cost]
        ]

-- * Journal audits

-- | Notes whose transactions have identical postings. The first note is the
-- smallest; the others follow in ascending order.
data DuplicateTransactions n = DuplicateTransactions n (NonEmpty n)
    deriving (Show, Eq)

-- | Report groups of notes whose postings are equal as multisets of value and
-- base. Posting order is ignored and values are compared exactly, without
-- rounding. The blank note ('ExchangeAlgebra.Journal.Core.plank') is
-- excluded. Groups are listed in ascending order of their first note.
-- Returns 'Warning' when any is found.
--
-- A journal entry posted twice keeps the trial balance in balance, so no
-- balance check can find it (Kieso, p. 2-21).
--
-- Legitimate exception: the same entry repeated on purpose, such as monthly
-- rent.
duplicateTransactions
    :: (HatVal v, ExBaseClass b, Note n)
    => Journal n v b
    -> AuditResult (JournalIssue n v b) (DuplicateTransactions n)
duplicateTransactions (input :: Journal n v b) = case journalStructureIssues input of
    issue : issues -> Error (StructureFinding issue :| map StructureFinding issues)
    []             -> warningFindings
        [ DuplicateTransactions first (next :| rest)
        | first : next : rest <- sortedGroups
        ]
  where
    groups :: M.Map [(v, b)] [n]
    groups = M.fromListWith (++)
        [ ( sort $ foldEntries (\entries value base -> (value, base) : entries) [] transaction
          , [note]
          )
        | (note, transaction) <- HM.toList (toMap input)
        , note /= plank
        ]
    sortedGroups :: [[n]]
    sortedGroups = sort $ map sort $ M.elems groups

-- | A posting to a revenue or expense account on the side opposite to its
-- home side.
data ContraryNominalPosting n v b = ContraryNominalPosting
    { _contraryNote  :: n  -- ^ The transaction.
    , _contraryValue :: v  -- ^ The posted value.
    , _contraryBase  :: b  -- ^ The posted base.
    }
    deriving (Show, Eq)

-- | Report, within each transaction, postings to revenue or expense accounts
-- on the side opposite to their home side. For contra accounts such as sales
-- allowances, whose home side is debit, a debit posting is normal and a credit
-- posting is reported. Transactions that post to an account reserved for
-- closing (income summary) are excluded. Returns 'Warning' when any is found.
--
-- Precondition: each note holds one transaction.
--
-- Legitimate exceptions: returns and allowances, reversing entries
-- (Kieso, p. 2-55), transfer of purchases to cost of sales, and elimination in
-- consolidation.
contraryNominalPostings
    :: (HatVal v, ExBaseClass b, Note n)
    => Journal n v b
    -> AuditResult (JournalIssue n v b) (ContraryNominalPosting n v b)
contraryNominalPostings (input :: Journal n v b) = case journalStructureIssues input of
    issue : issues -> Error (StructureFinding issue :| map StructureFinding issues)
    []             -> warningFindings
        [ ContraryNominalPosting note value base
        | (note, transaction) <- sortOn fst (HM.toList (toMap input))
        , note /= plank
        , let postings :: [(b, v)]
              postings = foldEntries (\entries value base -> (base, value) : entries) [] transaction
        , all ((/= ClosingOnly) . postingCapabilityFor . getAccountTitle . fst) postings
        , (base, value) <- sort postings
        , Just semantics <- [accountSemantics (getAccountTitle base)]
        , asemDivisionSemantics semantics `elem`
            [StatementDivision Revenue, StatementDivision Cost]
        , FixedHomeSide home <- [asemHomeSideSemantics semantics]
        , whichSide base /= home
        ]
