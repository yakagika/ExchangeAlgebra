{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Read CSV and generated entries with a model's own account vocabulary.
module Main (main) where

import Control.Monad (unless)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import ExchangeAlgebra.Accounting.Account
    ( AccountTitles(..)
    , AccountVocabulary(..)
    , AccountSpec(..)
    , ProcessingContext(..)
    , Side(..)
    , accountSpec
    , accountSemantics
    )
import ExchangeAlgebra.Accounting.Exchange (ExBaseClass(..))
import ExchangeAlgebra.Algebra.Base (BaseClass, Element(..), Hat(..), HatBase((:<)))
import ExchangeAlgebra.Algebra.Core (Alg, (.@), (.+))
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import qualified ExchangeAlgebra.IO.Input as Input
import qualified ExchangeAlgebra.IO.Input.Csv as Csv

-- | A firm's chart: built-in titles, a subscription-revenue title, and a
-- suspense title whose vocabulary gives no posting semantics. The built-in
-- wildcard is the chart's wildcard, so every other value is a concrete title
-- with a specification.
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
    vocabSpec SubscriptionRevenue = renamed "Subscription revenue" "定期便売上" ["subscriptions"]
    vocabSpec Suspense = renamed "Suspense" "仮勘定" []
    vocabSemantics (Std title) = accountSemantics title
    vocabSemantics SubscriptionRevenue = accountSemantics Sales
    vocabSemantics _ = Nothing
    closingTarget = Std RetainedEarnings

-- | A revenue specification under new names (so no name is shared with 'Sales').
renamed :: Text -> Text -> [Text] -> Maybe AccountSpec
renamed english label aliases = fmap
    (\spec -> spec { asNameEn = english, asNameJa = label, asLabelJa = label, asAliases = aliases })
    (accountSpec Sales)

type FirmEntry = Alg MoneyDecimal (HatBase FirmTitle)

-- | The names this firm's files use: English names, Japanese labels, aliases.
-- The wildcard in the list is dropped.
firmNames :: Input.AccountNameTable FirmTitle
firmNames = Input.vocabularyNames
    [Std Cash, Std Deposits, Std Sales, SubscriptionRevenue, Suspense, Std AccountTitle]

check :: Bool -> String -> IO ()
check ok label = unless ok (fail ("AccountNames: " ++ label))

main :: IO ()
main = do
    let resolve = Input.resolveWith firmNames

    -- CSV in the firm's own names.
    let csv = "side,account,amount\ndebit,Cash,3000\ncredit,定期便売上,3000\n"
        expected = 3000 .@ Not :< Std Cash .+ 3000 .@ Not :< SubscriptionRevenue :: FirmEntry
    check (Csv.parseJournalCsvBy resolve Csv.scientificAmount csv == Right expected)
          "CSV in the firm's names"

    -- A name given to two titles is ambiguous, never resolved by order.
    let shared = Input.extendAccountNames [("売上", SubscriptionRevenue)] firmNames
    check (Input.resolveWith shared "売上"
              == Left (Input.AmbiguousAccount "売上" [Std Sales, SubscriptionRevenue]))
          "shared name is ambiguous"

    -- The built-in table extended with a company's own label.
    let company = Input.extendAccountNames [("入金口座", Deposits)] Input.defaultAccountNames
        builtIn = "side,account,amount\ndebit,入金口座,500\ncredit,売上,500\n"
    check (Csv.parseJournalCsvBy (Input.resolveWith company) Csv.scientificAmount builtIn
              == Right (500 .@ Not :< Deposits .+ 500 .@ Not :< Sales
                        :: Alg MoneyDecimal (HatBase AccountTitles)))
          "built-in table with a company label"

    -- Generated entries: accepted, missing semantics, unknown name.
    check (Input.checkedEntryTextBy resolve OrdinaryJournal
              [("debit", "Cash", 3000), ("credit", "定期便売上", 3000)]
              == Right expected)
          "checked entry accepted"
    check (Input.checkedEntryTextBy resolve OrdinaryJournal
              [("debit", "Cash", 100), ("credit", "仮勘定", 100 :: MoneyDecimal)]
              == Left (Input.MissingPostingSemantics 1 Suspense :| []))
          "title without semantics rejected"
    check (Input.checkedEntryTextBy resolve OrdinaryJournal
              [("debit", "Cash", 100), ("credit", "Royalty", 100 :: MoneyDecimal)]
              == Left (Input.EntryParse 1 (Input.UnknownAccount "Royalty") :| []))
          "unknown name reported, balance still checked"

    -- A balanced batch with a vocabulary gap is certified, with the gap kept.
    let batch = [ ("tx1" :: Text, [("debit", "Cash", 100), ("credit", "Royalty", 100)])
                , ("tx2", [("debit", "Cash", 200), ("credit", "定期便売上", 200 :: MoneyDecimal)]) ]
    check (Input.certifyJournalTextBy resolve OrdinaryJournal batch
              == Input.BalancedUnresolved
                   { Input._certResolved =
                       [ ("tx1", [(Debit, Std Cash, 100)])
                       , ("tx2", [(Debit, Std Cash, 200), (Credit, SubscriptionRevenue, 200)]) ]
                   , Input._certUnresolved =
                       [ ("tx1", [(1, "Royalty", Input.UnknownAccount "Royalty")]) ]
                   , Input._certDebitTotal = 300
                   , Input._certCreditTotal = 300 })
          "balanced batch with an unknown name"

    putStrLn "[PASS] AccountNames: firm vocabulary for CSV and checked input"
