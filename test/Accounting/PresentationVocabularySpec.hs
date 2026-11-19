{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Presentation laws for empty defaults, reciprocal tables, and statement coverage.
module Accounting.PresentationVocabularySpec (runTests) where

import Control.Monad (forM_)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..), journalForSides)
import ExchangeAlgebra.Accounting.Statements.Presentation
import ExchangeAlgebra.Accounting.TrialBalance.Balance (AccountBalance(..))
import ExchangeAlgebra.Accounting.TrialBalance.Validation (TrialBalanceStage(..))
import ExchangeAlgebra.Algebra.Base (BaseClass, Element(..), HatBase(..))
import ExchangeAlgebra.Algebra.Core (Alg)
import Support (assertEqual)

-- | An ordinary vocabulary deliberately omitting the reciprocal-table method.
newtype DefaultTitle = DefaultTitle AccountTitles
    deriving (Eq, Ord, Show, Generic)

instance Hashable DefaultTitle
instance Element DefaultTitle where
    wildcard = DefaultTitle AccountTitle
instance BaseClass DefaultTitle

instance AccountVocabulary DefaultTitle where
    vocabSpec (DefaultTitle NetIncome) = accountSpec RetainedEarnings
    vocabSpec (DefaultTitle title) = accountSpec title
    vocabSemantics (DefaultTitle title) = case accountSemantics title of
        Just semantics | asemReportingEligibility semantics == DerivedPresentation ->
            accountSemantics RetainedEarnings
        other -> other
    closingTarget = DefaultTitle RetainedEarnings

instance ExBaseClass (HatBase DefaultTitle) where
    type TitleOf (HatBase DefaultTitle) = DefaultTitle
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | Independent metadata policies share only their coordinate representation.
class Typeable p => FixturePolicy p where
    fixturePairs :: Proxy p -> [(AccountTitles, AccountTitles)]
    fixturePairs _ = []
    fixtureSemantics :: Proxy p -> AccountTitles -> Maybe AccountSemantics
    fixtureSemantics _ = accountSemantics

newtype FixtureTitle (p :: Type) = FixtureTitle AccountTitles
    deriving (Eq, Ord, Show, Generic)

instance Hashable (FixtureTitle p)
instance Typeable p => Element (FixtureTitle p) where
    wildcard = FixtureTitle AccountTitle
instance Typeable p => BaseClass (FixtureTitle p)

instance FixturePolicy p => AccountVocabulary (FixtureTitle p) where
    vocabSpec (FixtureTitle title) = accountSpec title
    vocabSemantics (FixtureTitle title) = fixtureSemantics (Proxy :: Proxy p) title
    vocabReciprocalPairs =
        [(FixtureTitle left, FixtureTitle right) | (left, right) <- fixturePairs (Proxy :: Proxy p)]
    closingTarget = FixtureTitle RetainedEarnings

instance FixturePolicy p => ExBaseClass (HatBase (FixtureTitle p)) where
    type TitleOf (HatBase (FixtureTitle p)) = FixtureTitle p
    getAccountTitle (_ :< title) = title
    setAccountTitle (side :< _) title = side :< title

-- | The first pair is deliberately later in title order than the second pair.
data TwoPairs
instance FixturePolicy TwoPairs where
    fixturePairs _ = [(Deposits, AccountsPayable), (Cash, DepositsReceived)]
    fixtureSemantics _ title = fmap reciprocal (accountSemantics title)
      where
        reciprocal semantics
            | title `elem` [Deposits, AccountsPayable, Cash, DepositsReceived] =
                semantics { asemRoles = [ReciprocalAccount] }
            | otherwise = semantics

-- | Both roles are valid; only the Revenue endpoint violates the table law.
data NominalPair
instance FixturePolicy NominalPair where
    fixturePairs _ = [(Sales, AccountsPayable)]
    fixtureSemantics _ title = fmap reciprocal (accountSemantics title)
      where
        reciprocal semantics
            | title `elem` [Sales, AccountsPayable] =
                semantics { asemRoles = [ReciprocalAccount] }
            | otherwise = semantics

-- | Both statement divisions are valid; neither endpoint has the required role.
data RolelessPair
instance FixturePolicy RolelessPair where
    fixturePairs _ = [(Cash, AccountsPayable)]

data WildcardPair
instance FixturePolicy WildcardPair where
    fixturePairs _ = [(AccountTitle, AccountsPayable)]
    fixtureSemantics _ title = fmap reciprocal metadata
      where
        metadata = accountSemantics (if title == AccountTitle then Cash else title)
        reciprocal semantics = semantics { asemRoles = [ReciprocalAccount] }

-- | Eligibility alone cannot assign either balance to a statement section.
data NonStatement
instance FixturePolicy NonStatement where
    fixtureSemantics _ title = fmap nonStatement (accountSemantics title)
      where
        nonStatement semantics = case title of
            Cash -> semantics
                { asemReportingEligibility = StatementEligible
                , asemDivisionSemantics = DirectionEncoding Assets }
            AccountsPayable -> semantics
                { asemReportingEligibility = StatementEligible
                , asemDivisionSemantics = BookkeepingControlClass Liability }
            _ -> semantics

-- | A context with no implicit presentation instructions or label overrides.
context :: ReportingScope -> ReportingContextOf t Double
context scope = reportingContext JGAAP scope CanonicalEnglish

-- | Use the checked posting constructor with explicit debit/credit direction.
ledger
    :: ExBaseClass (HatBase t)
    => [(Side, t, Double)] -> Alg Double (HatBase t)
ledger = journalForSides

-- | Observe complete line identity, direction, and amount independently of labels.
lineBalances :: FinancialStatementsOf t Double -> [(t, Side, Double)]
lineBalances = map (\line -> (_lineAccount line, _lineSide line, _lineAmount line))
    . _statementLines

-- | Extract only reciprocal operations; mandatory metric events are separate.
reciprocalEvents :: FinancialStatementsOf t Double -> [PresentationAuditEventOf t Double]
reciprocalEvents = filter reciprocal . _presentationAudit
  where
    reciprocal (ReciprocalPairEliminated _ _ _ _) = True
    reciprocal _ = False

-- | Empty defaults retain branch-like coordinates and do not infer legacy metrics.
checkDefaults :: IO ()
checkDefaults = do
    assertEqual "L3 reciprocal table defaults to empty" []
        (vocabReciprocalPairs :: [(DefaultTitle, DefaultTitle)])
    let branch = DefaultTitle BranchCurrentAccount
        office = DefaultTitle HeadOfficeCurrentAccount
        source = ledger [(Debit, branch, 100), (Credit, office, 100)]
    forM_ [Standalone, Combined] $ \scope -> case present (context scope) BeforeClosing source of
        Left issues -> fail ("L3 default presentation failed: " ++ show issues)
        Right statements -> do
            assertEqual "L3 default table preserves both reciprocal-like lines"
                [(branch, Debit, 100), (office, Credit, 100)] (lineBalances statements)
            assertEqual "L3 empty table produces no reciprocal event" []
                (reciprocalEvents statements)
    assertEqual "L3 vocabulary has no derived-presentation metadata" []
        [title | title <- concreteAccountTitles,
                 Just semantics <- [vocabSemantics (DefaultTitle title)],
                 asemReportingEligibility semantics == DerivedPresentation]
    let resultTitle = DefaultTitle NetIncome
        cash = DefaultTitle Cash
        ordinaryResult = ledger [(Debit, cash, 7), (Credit, resultTitle, 7)]
    case present (context Standalone) BeforeClosing ordinaryResult of
        Left issues -> fail ("L3 ordinary legacy-equivalent title rejected: " ++ show issues)
        Right statements -> do
            assertEqual "L3 legacy-equivalent title remains a statement line" True
                (resultTitle `elem` map _lineAccount (_statementLines statements))
            assertEqual "L3 ordinary legacy-equivalent title has break-even metric"
                [NoBalance] (map _statementSubtotalBalance (_statementSubtotals statements))

-- | The declared table, rather than active-map position, identifies eliminated pairs.
checkTwoPairs :: IO ()
checkTwoPairs = do
    let aLeft = FixtureTitle Deposits :: FixtureTitle TwoPairs
        aRight = FixtureTitle AccountsPayable :: FixtureTitle TwoPairs
        bLeft = FixtureTitle Cash :: FixtureTitle TwoPairs
        bRight = FixtureTitle DepositsReceived :: FixtureTitle TwoPairs
        pairs = vocabReciprocalPairs :: [(FixtureTitle TwoPairs, FixtureTitle TwoPairs)]
        ends = concatMap (\(left, right) -> [left, right]) pairs
        source = ledger [(Debit, bLeft, 100), (Credit, bRight, 100)]
        expectedB = ReciprocalPairEliminated bLeft (DebitBalance 100)
            bRight (CreditBalance 100)
    assertEqual "L4 static reciprocal table preserves declared order"
        [(aLeft, aRight), (bLeft, bRight)] pairs
    assertEqual "L4 reciprocal endpoints are unique" (length ends) (length (nub ends))
    forM_ ends $ \title -> do
        assertEqual "L4 reciprocal endpoint is concrete" False (isWildcard title)
        assertEqual "L4 reciprocal endpoint has role and non-nominal division" True
            (case vocabSemantics title of
                Just semantics -> ReciprocalAccount `elem` asemRoles semantics
                    && asemDivisionSemantics semantics `notElem`
                        [StatementDivision Revenue, StatementDivision Cost]
                    && asemReportingEligibility semantics == StatementEligible
                Nothing -> False)
    case present (context Standalone) BeforeClosing source of
        Left issues -> fail ("L4 standalone presentation failed: " ++ show issues)
        Right statements -> do
            assertEqual "L4 standalone retains pair B lines"
                [(bLeft, Debit, 100), (bRight, Credit, 100)] (lineBalances statements)
            assertEqual "L4 standalone has no reciprocal events" [] (reciprocalEvents statements)
    case present (context Combined) BeforeClosing source of
        Left issues -> fail ("L4 combined presentation failed: " ++ show issues)
        Right statements -> do
            assertEqual "L4 combined removes both pair B lines" [] (lineBalances statements)
            assertEqual "L4 absent A cannot misidentify active pair B" [expectedB]
                (reciprocalEvents statements)
    let both = ledger [(Debit, aLeft, 20), (Credit, aRight, 20),
                       (Debit, bLeft, 100), (Credit, bRight, 100)]
    case present (context Combined) BeforeClosing both of
        Left issues -> fail ("L4 both-pair presentation failed: " ++ show issues)
        Right statements -> do
            assertEqual "L4 both pairs remove all four lines" [] (lineBalances statements)
            assertEqual "L4 reciprocal events follow table order"
                [ ReciprocalPairEliminated aLeft (DebitBalance 20) aRight (CreditBalance 20)
                , expectedB ] (reciprocalEvents statements)
    let instructed = (context Combined)
            { _presentationRelabels =
                [PresentationRelabel bLeft (FixtureTitle PettyCash) "explicit instruction"] }
    expectIssue "L4 pair endpoint conflicts with an instruction"
        (ConflictingPresentationInstruction bLeft) (present instructed BeforeClosing source)

-- | Each invalid fixture isolates one necessary validation condition.
checkInvalidPairs :: IO ()
checkInvalidPairs = do
    let nominal = FixtureTitle Sales :: FixtureTitle NominalPair
        nominalOther = FixtureTitle AccountsPayable :: FixtureTitle NominalPair
        nominalLedger = ledger [(Debit, nominal, 100), (Credit, nominalOther, 100)]
    expectIssue "L4 nominal endpoint rejected even with reciprocal role"
        (ConflictingPresentationInstruction nominal)
        (present (context Combined) BeforeClosing nominalLedger)
    let roleless = FixtureTitle Cash :: FixtureTitle RolelessPair
        rolelessOther = FixtureTitle AccountsPayable :: FixtureTitle RolelessPair
        rolelessLedger = ledger [(Debit, roleless, 100), (Credit, rolelessOther, 100)]
        rolelessResult = present (context Combined) AfterClosing rolelessLedger
    forM_ [roleless, rolelessOther] $ \title ->
        expectIssue "L4 non-nominal endpoint rejected without reciprocal role"
            (ConflictingPresentationInstruction title) rolelessResult
    let wildcardTitle = FixtureTitle AccountTitle :: FixtureTitle WildcardPair
        emptyLedger = mempty :: Alg Double (HatBase (FixtureTitle WildcardPair))
    expectIssue "L4 wildcard table endpoint rejected without wildcard postings"
        (ConflictingPresentationInstruction wildcardTitle)
        (present (context Combined) AfterClosing emptyLedger)

-- | Both balanced coordinates must be rejected when no statement division exists.
checkCoverage :: IO ()
checkCoverage = do
    let cash = FixtureTitle Cash :: FixtureTitle NonStatement
        payable = FixtureTitle AccountsPayable :: FixtureTitle NonStatement
        source = ledger [(Debit, cash, 100), (Credit, payable, 100)]
        result = present (context Standalone) BeforeClosing source
    expectIssue "L5 direction-only debit cannot be presented"
        (UnpresentableBalance cash (DebitBalance 100)) result
    expectIssue "L5 control-class credit cannot be presented"
        (UnpresentableBalance payable (CreditBalance 100)) result
    let targetContext = (context Standalone)
            { _presentationRelabels = [PresentationRelabel payable cash "control target"] }
    expectIssue "L5 non-statement target rejects relabeling"
        (ConflictingPresentationInstruction cash)
        (present targetContext BeforeClosing source)

-- | Require an exact issue while allowing independent additional diagnostics.
expectIssue
    :: (Eq t, Show t)
    => String
    -> PresentationIssueOf t Double
    -> Either (NE.NonEmpty (PresentationIssueOf t Double)) (FinancialStatementsOf t Double)
    -> IO ()
expectIssue label expected result = case result of
    Left issues -> assertEqual label True (expected `elem` NE.toList issues)
    Right statements -> fail (label ++ ": unexpected statement " ++ show statements)

-- | Register independent presentation-vocabulary laws.
runTests :: IO ()
runTests = do
    checkDefaults
    checkTwoPairs
    checkInvalidPairs
    checkCoverage
