{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Name-table laws and user-vocabulary input boundary regressions.
module IO.AccountNamesSpec (runTests) where

import Control.Monad (forM_)
import Data.Hashable (Hashable)
import Data.List (delete, nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.QuickCheck (Gen, chooseInt, elements, forAll, listOf, vectorOf)

import ExchangeAlgebra.Accounting.Account
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..), journalForSides)
import ExchangeAlgebra.Algebra.Base (BaseClass, Element(..), Hat(..), HatBase((:<)))
import ExchangeAlgebra.Algebra.Core (Alg, (.@), Redundant((.+)))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.IO.Input as Input
import qualified ExchangeAlgebra.IO.Input.Csv as Csv
import Support (assertEqual, quickProp)

-- | Own all instances locally, including the exchange base, to avoid orphans.
data FirmTitle
    = Std AccountTitles
    | SubscriptionRevenue
    | Suspense
    deriving (Eq, Ord, Show, Generic)

instance Hashable FirmTitle

instance Element FirmTitle where
    wildcard = Std AccountTitle

instance BaseClass FirmTitle

instance ExBaseClass (HatBase FirmTitle) where
    type TitleOf (HatBase FirmTitle) = FirmTitle
    getAccountTitle (_ :< title) = title
    setAccountTitle (direction :< _) title = direction :< title

instance AccountVocabulary FirmTitle where
    vocabSpec (Std title) = accountSpec title
    vocabSpec SubscriptionRevenue =
        renamed "Subscription revenue" "定期便売上" ["subscriptions", "売上"]
    vocabSpec Suspense = renamed "Suspense" "仮勘定" []
    vocabSemantics (Std title) = accountSemantics title
    vocabSemantics SubscriptionRevenue = accountSemantics Sales
    vocabSemantics Suspense = Nothing
    closingTarget = Std RetainedEarnings

-- | Keep the revenue classification and replace its external names.
renamed :: Text -> Text -> [Text] -> Maybe AccountSpec
renamed english label aliases = fmap
    (\spec -> spec { asNameEn = english, asNameJa = label
                   , asLabelJa = label, asAliases = aliases })
    (accountSpec Sales)

-- | Represent an entry in the test chart with exact decimal amounts.
type FirmEntry = Alg MoneyDecimal (HatBase FirmTitle)

-- | Include the wildcard in the input list to exercise its exclusion.
firmNames :: Input.AccountNameTable FirmTitle
firmNames = Input.vocabularyNames
    [Std AccountTitle, Std Cash, Std Sales, SubscriptionRevenue, Suspense]

-- | Look up names in the test chart.
resolve :: Input.AccountResolver FirmTitle
resolve = Input.resolveWith firmNames

-- | Deliberately list-based reference: preserve first occurrence of each candidate.
reference :: Eq a => (Text -> Text) -> [(Text, a)] -> Input.AccountResolver a
reference normalize pairs query = case nub
    [title | (name, title) <- pairs, normalize name == normalize query] of
        [] -> Left (Input.UnknownAccount query)
        [title] -> Right title
        titles -> Left (Input.AmbiguousAccount query titles)

-- | Draw from a small alphabet with normalization collisions.
smallName :: Gen Text
smallName = elements ["", "a", "A", "b", "B", " a ", "xA", "売上", "shared"]

-- | Generate repeated names and repeated candidates independently.
namePairs :: Gen [(Text, Int)]
namePairs = listOf ((,) <$> smallName <*> chooseInt (0, 3))

-- | Exercise exact, case-insensitive, and non-idempotent normalization.
normalizers :: [(String, Text -> Text)]
normalizers = [("exact", id), ("lower", T.toLower), ("prefix", ("x" <>))]

-- | Compare construction and extension against ordered input pairs.
nameTableLaws :: IO ()
nameTableLaws = forM_ normalizers $ \(label, normalize) -> do
    quickProp ("T1 ordered unique candidates: " ++ label) $
        forAll namePairs $ \pairs -> forAll smallName $ \query ->
            let queries = query : map fst pairs
                actual = Input.resolveWith (Input.accountNameTable normalize pairs)
            in all (\q -> actual q == reference normalize pairs q) queries
    quickProp ("T2 extension equals append: " ++ label) $
        forAll namePairs $ \old -> forAll namePairs $ \new ->
        forAll smallName $ \query ->
            let extended = Input.resolveWith
                    (Input.extendAccountNames new (Input.accountNameTable normalize old))
                appended = Input.resolveWith (Input.accountNameTable normalize (old ++ new))
            in all (\q -> extended q == appended q) (query : map fst (old ++ new))

-- | Reconstruct the historical candidate order for all built-in names.
legacyLaws :: IO ()
legacyLaws = do
    let titles = concreteAccountTitles
        aliasOrder = CommunicationExpenses : delete CommunicationExpenses titles
        pairs = [(T.pack (show title), title) | title <- titles]
             ++ [(alias, title) | title <- aliasOrder, alias <- accountAliases title]
        queries = "AccountTitle" : map fst pairs
        expected = reference Input.normalizeTitle pairs
        expectedAll = [(query, expected query) | query <- queries]
    assertEqual "T3 legacy parseAccountTitle, all known names" expectedAll
        [(query, Input.parseAccountTitle query) | query <- queries]
    assertEqual "T3 default table matches legacy reference, all known names" expectedAll
        [(query, Input.resolveWith Input.defaultAccountNames query) | query <- queries]
    quickProp "T3 legacy reference on generated and known queries" $
        forAll (elements queries) $ \known -> forAll smallName $ \randomQuery ->
            all (\q -> Input.parseAccountTitle q == expected q
                && Input.resolveWith Input.defaultAccountNames q == expected q)
                [known, randomQuery, "  " <> T.toUpper known <> "  "]

-- | Check the names exposed by each listed title and its specification.
vocabularyLaws :: IO ()
vocabularyLaws = do
    let titles = [Std Cash, Std Sales, SubscriptionRevenue, Suspense]
        names title = T.pack (show title) : case vocabSpec title of
            Just spec -> asNameEn spec : asLabelJa spec : asAliases spec
            Nothing -> []
        entries = [(name, title) | title <- titles, name <- names title]
    forM_ entries $ \(name, _) ->
        assertEqual "T4 every listed title's show, English name, Japanese label and aliases"
            (reference Input.normalizeTitle entries name) (resolve name)
    forM_ ["SubscriptionRevenue", "Subscription revenue", "定期便売上", "subscriptions"] $
        \name -> assertEqual "T4 vocabulary names" (Right SubscriptionRevenue) (resolve name)
    assertEqual "T4 wrapped built-in show" (Right (Std Cash)) (resolve "Std Cash")
    assertEqual "T4 wrapped English name" (Right (Std Cash)) (resolve "Cash")
    assertEqual "T4 wrapped Japanese label" (Right (Std Cash)) (resolve "現金")
    assertEqual "T4 wildcard excluded" (Left (Input.UnknownAccount "Std AccountTitle"))
        (resolve "Std AccountTitle")
    assertEqual "T4 vocabulary specs share a label in list order"
        (Left (Input.AmbiguousAccount "売上" [Std Sales, SubscriptionRevenue])) (resolve "売上")

-- | Compare CSV rows with their algebra projection and check parser errors.
csvLaws :: IO ()
csvLaws = do
    quickProp "T5 CSV term is the projection of noted rows, preserving repetition" $
        forAll (chooseInt (0, 12)) $ \count ->
        forAll (vectorOf count ((,) <$> elements [Debit, Credit] <*> chooseInt (0, 100))) $
        \rows ->
            let triples = [(side, Std Cash, fromIntegral amount :: MoneyDecimal)
                          | (side, amount) <- rows]
                csv = T.unlines ("side,account,amount,note" :
                    [T.intercalate "," [sideText side, "Cash", T.pack (show amount), "note"]
                    | (side, amount) <- rows])
                expected = Right (journalForSides triples :: FirmEntry)
            in Csv.parseJournalCsvBy resolve Csv.scientificAmount csv == expected
                && fmap (journalForSides . map dropNote)
                    (Csv.parseNotedJournalCsvBy resolve Csv.scientificAmount csv) == expected
                && (Csv.parseJournalCsvBy Input.parseAccountTitle Csv.scientificAmount csv
                        == (Csv.parseJournalCsvWith Csv.scientificAmount csv
                            :: Either Input.ConvError (Alg MoneyDecimal (HatBase AccountTitles))))
                && (Csv.parseNotedJournalCsvBy Input.parseAccountTitle Csv.scientificAmount csv
                        == (Csv.parseNotedJournalCsv Csv.scientificAmount csv
                            :: Either Input.ConvError [(Side, AccountTitles, MoneyDecimal, Text)]))
    let row name = "side,account,amount,note\ndebit," <> name <> ",0,zero\n"
        wildcardResolver _ = Right (Std AccountTitle)
    assertEqual "T5 wildcard CSV field is trimmed"
        (Left (Input.UnknownAccount "wild"))
        (Csv.parseJournalCsvBy wildcardResolver Csv.scientificAmount (row " wild ")
            :: Either (Input.ConvErrorOf FirmTitle) FirmEntry)
    assertEqual "T5 wildcard noted CSV field is trimmed"
        (Left (Input.UnknownAccount "wild"))
        (Csv.parseNotedJournalCsvBy wildcardResolver Csv.scientificAmount (row " wild ")
            :: Either (Input.ConvErrorOf FirmTitle) [(Side, FirmTitle, MoneyDecimal, Text)])
    forM_ [-1, 1 / 0 :: Double] $ \bad -> do
        let amount _ = Right bad
            csv = "side,account,amount\ndebit,Cash, raw \n"
            expected = Left (Input.BadAmount "raw")
        assertEqual "T5 custom invalid amount in term CSV" expected
            (Csv.parseJournalCsvBy resolve amount csv)
        assertEqual "T5 custom invalid amount in noted CSV" expected
            (Csv.parseNotedJournalCsvBy resolve amount csv)
    assertEqual "T5 zero row retains note"
        (Right [(Debit, Std Cash, 0 :: MoneyDecimal, "zero")])
        (Csv.parseNotedJournalCsvBy resolve Csv.scientificAmount (row "Cash"))
    assertEqual "T5 zero row contributes no posting" (Right (mempty :: FirmEntry))
        (Csv.parseJournalCsvBy resolve Csv.scientificAmount (row "Cash"))
  where
    dropNote (side, title, amount, _) = (side, title, amount)
    sideText Debit = "debit"
    sideText Credit = "credit"
    sideText Side = "Side"

-- | Check row diagnostics and balance using the test chart.
checkedLaws :: IO ()
checkedLaws = do
    let check = Input.checkedEntryTextBy resolve OrdinaryJournal
        wildcardResolve = Input.resolveWith
            (Input.extendAccountNames [("wild", Std AccountTitle)] firmNames)
        expected = 100 .@ Not :< Std Cash .+ 100 .@ Not :< SubscriptionRevenue :: FirmEntry
    assertEqual "T6 own vocabulary accepted" (Right expected)
        (check [("debit", "Cash", 100), ("credit", "subscriptions", 100)])
    assertEqual "T6 missing semantics follows amount error"
        (Left (Input.NonPositiveAmount 0 Suspense 0 :|
            [ Input.MissingPostingSemantics 0 Suspense
            , Input.NonPositiveAmount 1 Suspense 0
            , Input.MissingPostingSemantics 1 Suspense ]))
        (check [("debit", "Suspense", 0), ("credit", "Suspense", 0)])
    assertEqual "T6 resolved wildcard is structural"
        (Left (Input.WildcardAccount 0 :| []))
        (Input.checkedEntryTextBy wildcardResolve OrdinaryJournal
            [("debit", "wild", 100), ("credit", "Cash", 100 :: MoneyDecimal)])
    assertEqual "T6 unresolved amounts still enter balance"
        (Left (Input.EntryParse 1 (Input.UnknownAccount "unknown") :| [Input.Imbalanced 100 90]))
        (check [("debit", "Cash", 100), ("credit", "unknown", 90)])
    let invalid = [("invalid", "Suspense", 0 :: MoneyDecimal)]
    assertEqual "T6 invalid side suppresses checked structural checks"
        (Left (Input.EntryParse 0 (Input.UnknownSide "invalid") :| [])) (check invalid)
    assertEqual "T6 certification checks a row even with invalid side"
        (Input.Rejected (Input.EntryErrors ("invalid" :: Text)
            (Input.EntryParse 0 (Input.UnknownSide "invalid") :|
                [ Input.NonPositiveAmount 0 Suspense 0
                , Input.MissingPostingSemantics 0 Suspense ]) :| []))
        (Input.certifyJournalTextBy resolve OrdinaryJournal [("invalid", invalid)])

-- | Distinguish vocabulary gaps from structural and per-entry balance errors.
certificationLaws :: IO ()
certificationLaws = do
    let shared = Input.resolveWith
            (Input.extendAccountNames [("shared", Std Cash), ("shared", SubscriptionRevenue)]
                firmNames)
        certify = Input.certifyJournalTextBy shared OrdinaryJournal
        batch =
            [ ("z", [("debit", "unknown", 10), ("credit", "shared", 10)])
            , ("a", [("debit", "Cash", 20), ("credit", "subscriptions", 20)])
            , ("m", [("debit", "Cash", 30), ("credit", "other", 30)])
            ] :: [(Text, [(Text, Text, MoneyDecimal)])]
    assertEqual "T7 unresolved groups keep transaction and row order; omit empty groups"
        (Input.BalancedUnresolved
            [("a", [(Debit, Std Cash, 20), (Credit, SubscriptionRevenue, 20)])
            ,("m", [(Debit, Std Cash, 30)])]
            [("z", [(0, "unknown", Input.UnknownAccount "unknown")
                   ,(1, "shared", Input.AmbiguousAccount "shared"
                       [Std Cash, SubscriptionRevenue])])
            ,("m", [(1, "other", Input.UnknownAccount "other")])]
            60 60)
        (certify batch)
    let wildcardResolve = Input.resolveWith
            (Input.extendAccountNames [("wild", Std AccountTitle)] firmNames)
    assertEqual "T7 wildcard is rejected rather than unresolved"
        (Input.Rejected (Input.EntryErrors ("wild" :: Text)
            (Input.WildcardAccount 0 :| []) :| []))
        (Input.certifyJournalTextBy wildcardResolve OrdinaryJournal
            [("wild", [("debit", "wild", 10), ("credit", "Cash", 10 :: MoneyDecimal)])])
    let badResolver _ = Left (Input.BadAmount "resolver")
        row = [("debit", "anything", 10 :: MoneyDecimal)]
    assertEqual "T7 non-vocabulary resolver failure is EntryParse"
        (Left (Input.EntryParse 0 (Input.BadAmount "resolver") :| [Input.Imbalanced 10 0]))
        (Input.checkedEntryTextBy badResolver OrdinaryJournal row
            :: Either (NonEmpty (Input.EntryErrorOf FirmTitle MoneyDecimal)) FirmEntry)
    assertEqual "T7 non-vocabulary resolver failure rejects certification"
        (Input.Rejected (Input.EntryErrors ("bad" :: Text)
            (Input.EntryParse 0 (Input.BadAmount "resolver") :| []) :| []))
        (Input.certifyJournalTextBy (badResolver :: Input.AccountResolver FirmTitle)
            OrdinaryJournal [("bad", row)])
    assertEqual "T7 literal Side is wildcard side"
        (Input.Rejected (Input.EntryErrors ("side" :: Text)
            (Input.WildcardSide 0 :| []) :| []))
        (certify [("side", [("Side", "Cash", 10 :: MoneyDecimal)])])
    assertEqual "T7 separate transaction imbalances cannot cancel"
        (Input.Rejected
            (Input.EntryErrors ("debit" :: Text) (Input.Imbalanced 10 0 :| []) :|
            [Input.EntryErrors "credit" (Input.Imbalanced 0 10 :| [])]))
        (certify [("debit", [("debit", "unknown", 10 :: MoneyDecimal)])
                 ,("credit", [("credit", "unknown", 10)])])

-- | Check each concrete title in every posting context against the registry.
postingPolicyLaw :: IO ()
postingPolicyLaw =
    assertEqual "T8 checked capability, all concrete titles and contexts" [] failures
  where
    failures =
        [ (title, context, expected, actual)
        | title <- concreteAccountTitles
        , context <- [OrdinaryJournal, ClosingProcess, ConsolidationWorksheet, EngineComputation]
        , let capability = postingCapabilityFor title
              name = T.pack (show title)
              rows = [("debit", name, 1), ("credit", name, 1)]
              expected
                  | postingAllowedIn context capability = Right
                      (journalForSides [(Debit, title, 1), (Credit, title, 1)])
                  | otherwise = Left
                      (Input.PostingNotAllowed 0 title capability context :|
                          [Input.PostingNotAllowed 1 title capability context])
              actual = Input.checkedEntryTextBy Input.parseAccountTitle context rows
                  :: Either (NonEmpty (Input.EntryError MoneyDecimal))
                      (Alg MoneyDecimal (HatBase AccountTitles))
        , expected /= actual
        ]

-- | Run generated lookup laws before input and posting-policy regressions.
runTests :: IO ()
runTests = do
    nameTableLaws
    legacyLaws
    vocabularyLaws
    csvLaws
    checkedLaws
    certificationLaws
    postingPolicyLaw
