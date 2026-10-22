{-# LANGUAGE OverloadedStrings #-}

-- | Public-format fixtures and laws J1-J3 for the explicit admission decoders.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.ByteString.Lazy.Char8 as Ascii
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

import ExchangeAlgebra.Aeson.Admission
import ExchangeAlgebra.Accounting.Account (AccountTitles(..))
import ExchangeAlgebra.Algebra.Value (MoneyParseError(..))
import qualified ExchangeAlgebra.IO.Input.Admission as A

-- | Fail with the assertion name and both observations.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual = unless (expected == actual)
    (fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

-- | Require success rather than allowing two failed paths to count as equality.
right :: Show problem => Either problem a -> IO a
right = either (fail . show) pure

-- | Load synthetic fixtures from this package's test directory.
fixture :: FilePath -> IO ByteString
fixture name = Bytes.readFile ("test/fixtures/" ++ name ++ ".json")

-- | Compare the complete error collection without imposing an error order.
failures :: String -> [DecodeFailure] -> Either (NonEmpty.NonEmpty DecodeFailure) a -> IO ()
failures label expected outcome = case outcome of
    Right _ -> fail (label ++ ": unexpectedly accepted")
    Left problems -> do
        let actual = NonEmpty.toList problems
        assertEqual (label ++ " count") (length expected) (length actual)
        unless (all (`elem` actual) expected) (fail (label ++ ": " ++ show actual))

-- | A full key with a fixed synthetic entity and period.
key :: Text -> Text -> A.TxKey
key entity transaction = A.TxKey (A.EntityId entity) (A.PeriodId "2026") (A.TxId transaction)

-- | J1: the documented shapes decode to independently written public values.
j1 :: IO ()
j1 = do
    decoded <- fixture "valid/admission" >>= right . decodeAdmissionSpec
    expectedRegistry <- right $ A.txIdRegistry
        [ (key "A" "t1", A.txRule A.Required [A.SupplySubmission A.Ordinary]
                                  (Just (A.EvidenceId "e1")))
        , (key "A" "t0", A.txRule A.Required [A.SupplyFacts (A.FactId "f1") A.Opening] Nothing)
        , (key "A" "close", A.txRule A.Optional [A.SupplyCatalog A.FinalStockKind] Nothing)
        ]
    assertEqual "J1 registry" (A.registryRules expectedRegistry)
        $ A.registryRules
        $ A.admissionRegistry decoded
    assertEqual "J1 evidence" (Map.singleton (A.EvidenceId "e1") 120.50)
                              (A.admissionEvidence decoded)
    assertEqual "J1 facts" (Map.singleton (A.FactId "f1") [("debit", "Cash", 100)])
                           (A.admissionFacts decoded)
    assertEqual "J1 vocabulary" (Set.fromList [Cash, Sales]) (A.admissionVocabulary decoded)
    submitted <- fixture "valid/submission" >>= right . decodeSubmissionPostings
    let expected = A.Submission [(key "A" "t1", [("debit", "Cash", 25)])] []
    assertEqual "J1 submission" expected submitted
    repeated <- right $ decodeSubmissionPostings $
        "{\"format\":\"exchangealgebra-submission/dev\",\"postings\":[{\"entity\":\"A\",\
        \\"period\":\"2026\",\"tx\":\"t1\",\"rows\":[{\"side\":\"debit\",\"account\":\"Cash\",\
        \\"amount\":25},{\"side\":\"debit\",\"account\":\"Cash\",\"amount\":25}]},\
        \{\"entity\":\"A\",\"period\":\"2026\",\"tx\":\"t1\",\"rows\":[]}]}"
    assertEqual "J1 posting order and redundancy"
        (A.Submission [(key "A" "t1", [("debit", "Cash", 25), ("debit", "Cash", 25)]),
                       (key "A" "t1", [])] []) repeated

-- | Test arbitrary JSON fragments in an otherwise complete trusted specification.
specWith :: Text -> Text -> ByteString
specWith registry vocabulary = Bytes.fromStrict $ Encoding.encodeUtf8
    ("{\"format\":\"exchangealgebra-admission-spec/dev\",\"registry\":" <> registry <>
     ",\"evidence\":[],\"facts\":[],\"vocabulary\":" <> vocabulary <> "}")

-- | One supply in a complete registry declaration.
ruleWith :: Text -> Text -> Text
ruleWith presence supply = "[{\"entity\":\"A\",\"period\":\"2026\",\"tx\":\"t\",\"presence\":\"" <>
    presence <> "\",\"supplies\":[" <> supply <> "],\"evidence\":null}]"

-- | J2: every specified refusal, including independent siblings and repeated keys.
j2 :: IO ()
j2 = do
    let amountPath = [Field "evidence", Index 0, Field "amount"]
        postingPath = [Field "postings", Index 0]
    forM_
        [ ("unknown-field", [UnknownField [Field "extra"] "extra"])
        , ("negative-money", [BadMoney amountPath NegativeMoney])
        , ("exponent-money", [BadMoney amountPath MoneyExponentOutOfRange])
        , ("scale-money", [BadMoney amountPath MoneyScaleOutOfRange])
        , ("account-alias", [BadAccount [Field "vocabulary", Index 0] "売掛金"])
        , ("duplicate-evidence",
            [ DuplicateKey [Field "evidence", Index 0, Field "id"] "e"
            , DuplicateKey [Field "evidence", Index 1, Field "id"] "e"
            , WrongType [Field "evidence", Index 1, Field "amount"] "number"
            ])
        , ("duplicate-facts",
            [DuplicateKey [Field "facts", Index 0, Field "id"] "f",
             DuplicateKey [Field "facts", Index 1, Field "id"] "f"])
        ] $ \(name, expected) -> fixture ("reject/" ++ name) >>= failures name expected
            . decodeAdmissionSpec
    forM_
        [ ("missing-field", [MissingField [Field "postings"] "postings"])
        , ("format-missing", [MissingField [Field "format"] "format"])
        , ("format-type", [WrongType [Field "format"] "string"])
        , ("format-unknown", [UnsupportedFormat "v999"])
        , ("format-duplicate", [DuplicateKey [Field "format"] "format"])
        , ("duplicate-key", [DuplicateKey (postingPath ++ [Field "rows"]) "rows"])
        , ("blank-id", [BadIdentifier (postingPath ++ [Field "entity"]) " \t"])
        , ("multiple-errors",
            [ WrongType (postingPath ++ [Field "entity"]) "string"
            , BadIdentifier (postingPath ++ [Field "period"]) ""
            , WrongType (postingPath ++ [Field "rows"]) "array"
            , BadMoney [Field "postings", Index 1, Field "rows", Index 0, Field "amount"]
                       NegativeMoney
            , WrongType [Field "postings", Index 1, Field "rows", Index 1, Field "amount"] "number"
            ])
        ] $ \(name, expected) -> fixture ("reject/" ++ name) >>= failures name expected
            . decodeSubmissionPostings
    let registryKey = key "shop" "t"
    fixture "reject/registry" >>= failures "registry"
        [RegistryRejected (A.MixedFactSupply registryKey NonEmpty.:|
                           [A.FactEvidenceForbidden registryKey])] . decodeAdmissionSpec
    let supplyPath = [Field "registry", Index 0, Field "supplies", Index 0]
        badSupply = "{\"kind\":false,\"role\":false,\"fact\":false,\"operation\":false}"
    failures "mistyped discriminator siblings"
        [WrongType (supplyPath ++ [Field name]) "string"
        | name <- ["kind", "role", "fact", "operation"]]
        $ decodeAdmissionSpec
        $ specWith (ruleWith "required" badSupply) "[]"
    broken <- fixture "reject/malformed"
    forM_ [broken, "{} trailing", "{}{}", "[1,]", "{\"format\":\"x\",\"x\":[}"] $ \bytes ->
        case decodeSubmissionPostings bytes of
            Left (MalformedJson _ NonEmpty.:| []) -> pure ()
            other -> fail ("malformed: " ++ show other)
    forM_ ["1e18446744073709551616", "1e-18446744073709551616", "0.1e999999999"] $ \amount ->
        case decodeAdmissionSpec (moneyDocument amount) of
            Left (MalformedJson _ NonEmpty.:| []) -> pure ()
            Left (BadMoney _ MoneyExponentOutOfRange NonEmpty.:| []) -> pure ()
            _ -> fail ("huge exponent accepted or misclassified: " ++ Ascii.unpack amount)
    forM_ ["1e9999999999", "quote\\\"1e9999999999", "slash\\\\1e9999999999"] $ \name -> do
        let bytes = "{\"format\":\"exchangealgebra-submission/dev\",\"postings\":[{\"entity\":\"" <>
                    name <> "\",\"period\":\"2026\",\"tx\":\"t\",\"rows\":[]}]}"
        _ <- right (decodeSubmissionPostings bytes)
        pure ()
    precise <- right
             $ decodeAdmissionSpec
             $ moneyDocument "0.1000000000000000000000000001"
    assertEqual "exact decimal" (Map.singleton (A.EvidenceId "e")
        0.1000000000000000000000000001) (A.admissionEvidence precise)
    allAccounts <- right $ decodeAdmissionSpec $ specWith "[]"
        ("[" <> Text.intercalate "," ["\"" <> Text.pack (show title) <> "\""
                                    | title <- [minBound .. maxBound :: AccountTitles]] <> "]")
    assertEqual "canonical vocabulary" (Set.fromList [minBound .. maxBound])
                                       (A.admissionVocabulary allAccounts)
    forM_ ["entity", "period", "tx"] $ \name -> do
        let bytes = "{\"format\":\"exchangealgebra-submission/dev\",\"postings\":[{" <>
                    Text.intercalate "," ["\"" <> fieldName <> "\":\"" <> value <> "\""
                        | fieldName <- ["entity", "period", "tx"],
                          let value = blankOrName name fieldName] <> ",\"rows\":[]}]}"
        failures "blank key" [BadIdentifier (postingPath ++ [Field name]) " "]
            $ decodeSubmissionPostings
            $ Bytes.fromStrict
            $ Encoding.encodeUtf8 bytes
    forM_ ["evidence", "facts"] $ \name -> do
        let valueField = case name of
                "evidence" -> "\"amount\":1"
                _ -> "\"rows\":[]"
            bytes = Text.replace ("\"" <> name <> "\":[]")
                                 ("\"" <> name <> "\":[{\"id\":\" \"," <> valueField <> "}]")
                  $ Encoding.decodeUtf8
                  $ Bytes.toStrict (specWith "[]" "[]")
        failures "blank map id" [BadIdentifier [Field name, Index 0, Field "id"] " "]
            $ decodeAdmissionSpec
            $ Bytes.fromStrict
            $ Encoding.encodeUtf8 bytes
  where
    moneyDocument :: ByteString -> ByteString
    moneyDocument amount = "{\"format\":\"exchangealgebra-admission-spec/dev\",\"registry\":[]," <>
        "\"evidence\":[{\"id\":\"e\",\"amount\":" <> amount <> "}],\"facts\":[],\"vocabulary\":[]}"
    blankOrName :: Text -> Text -> Text
    blankOrName selected name
        | selected == name = " "
        | otherwise = name

-- | Exhaustive role spellings: a new core constructor must be handled here.
roleName :: A.Role -> Text
roleName A.Ordinary = "ordinary"
roleName A.Opening = "opening"
roleName A.Adjustment = "adjustment"
roleName A.Closing = "closing"
roleName A.Elimination = "elimination"

-- | Exhaustive operation classification for the registry's query-only refusals.
isQuery :: A.CatalogOperationKind -> Bool
isQuery operation = case operation of
    A.CogsKind -> False
    A.DepIndirectKind -> False
    A.DepDirectKind -> False
    A.AllowanceKind -> False
    A.AllowanceRateKind -> False
    A.AllowanceResetKind -> False
    A.PrepaidKind -> False
    A.UnearnedKind -> False
    A.AccruedRevenueKind -> False
    A.AccruedExpenseKind -> False
    A.ReverseEntryKind -> False
    A.ConsumptionTaxKind -> False
    A.CorporateInterimKind -> False
    A.CorporateSettlementKind -> False
    A.EquityEarningsKind -> False
    A.EquityDividendKind -> False
    A.EquityEntriesKind -> False
    A.EquityBalanceKind -> True
    A.PriorErrorKind -> False
    A.FinalStockKind -> False
    A.StraightLineKind -> False
    A.ConsolidateKind -> True

-- | Exercise every constructor in the enums without Enum/Bounded instances.
closedEnums :: IO ()
closedEnums = do
    forM_ [A.Required, A.Optional] $ \presence ->
        forM_ [A.Ordinary, A.Opening, A.Adjustment, A.Closing, A.Elimination] $ \role -> do
            let supply = "{\"kind\":\"submission\",\"role\":\"" <> roleName role <> "\"}"
                presenceName = case presence of
                    A.Required -> "required"
                    A.Optional -> "optional"
            decoded <- right $ decodeAdmissionSpec $ specWith (ruleWith presenceName supply) "[]"
            assertEqual "presence and role" [(key "A" "t", A.txRule presence
                [A.SupplySubmission role] Nothing)]
                $ Map.toList
                $ A.registryRules
                $ A.admissionRegistry decoded
    forM_
        [ A.CogsKind, A.DepIndirectKind, A.DepDirectKind, A.AllowanceKind
        , A.AllowanceRateKind, A.AllowanceResetKind, A.PrepaidKind, A.UnearnedKind
        , A.AccruedRevenueKind, A.AccruedExpenseKind, A.ReverseEntryKind, A.ConsumptionTaxKind
        , A.CorporateInterimKind, A.CorporateSettlementKind, A.EquityEarningsKind
        , A.EquityDividendKind
        , A.EquityEntriesKind, A.EquityBalanceKind, A.PriorErrorKind, A.FinalStockKind
        , A.StraightLineKind, A.ConsolidateKind
        ] $ \operation -> do
            let supply = "{\"kind\":\"catalog\",\"operation\":\"" <>
                         Text.pack (show operation) <> "\"}"
                decoded = decodeAdmissionSpec
                        $ specWith (ruleWith "optional" supply) "[]"
            case isQuery operation of
                True -> failures "query kind" [RegistryRejected
                    (A.NonGeneratingSupply (key "A" "t") operation NonEmpty.:| [])] decoded
                False -> do
                    spec <- right decoded
                    assertEqual "operation" [(key "A" "t", A.txRule A.Optional
                        [A.SupplyCatalog operation] Nothing)]
                        $ Map.toList
                        $ A.registryRules
                        $ A.admissionRegistry spec
    forM_ [("presence", "{\"kind\":\"submission\",\"role\":\"ordinary\"}"),
           ("required", "{\"kind\":\"submission\",\"role\":\"unknown\"}"),
           ("required", "{\"kind\":\"unknown\"}"),
           ("required", "{\"kind\":\"catalog\",\"operation\":\"unknown\"}")]
        $ \(presence, supply) ->
        case decodeAdmissionSpec $ specWith (ruleWith presence supply) "[]" of
            Left (WrongType _ _ NonEmpty.:| []) -> pure ()
            _ -> fail "unknown enumeration was not WrongType"

-- | J3: both public input routes admit successfully and have identical observations.
j3 :: IO ()
j3 = do
    registry <- right $ A.txIdRegistry
        [ (key "shop" "sale", A.txRule A.Required [A.SupplySubmission A.Ordinary]
                                      (Just (A.EvidenceId "sale-total")))
        , (key "shop" "expense", A.txRule A.Required [A.SupplySubmission A.Ordinary]
                                         (Just (A.EvidenceId "expense-total")))
        ]
    let expectedSpec = A.AdmissionSpec registry
            (Map.fromList [(A.EvidenceId "sale-total", 100), (A.EvidenceId "expense-total", 100)])
            Map.empty (Set.fromList [Cash, Sales, RentExpense])
        expectedSubmission = A.Submission
            [ (key "shop" "sale", [("Debit", "Cash", 100), ("Credit", "Sales", 100)])
            , (key "shop" "expense", [("Debit", "RentExpense", 100), ("Credit", "Cash", 100)])
            ] []
    spec       <- fixture "valid/admit-spec" >>= right . decodeAdmissionSpec
    submission <- fixture "valid/admit-submission" >>= right . decodeSubmissionPostings
    expected   <- right (A.admit expectedSpec expectedSubmission)
    actual     <- right (A.admit spec submission)
    assertEqual "J3 ledger" (A.deriveLedger expected) (A.deriveLedger actual)
    expectedTrial <- right (A.deriveTrialBalance expected)
    actualTrial   <- right (A.deriveTrialBalance actual)
    assertEqual "J3 balances" (A.admittedTrialBalance expectedTrial)
                              (A.admittedTrialBalance actualTrial)

-- | Run the three laws and the closed enumeration checks.
main :: IO ()
main = do
    j1
    j2
    closedEnums
    j3
    putStrLn "J1, J2, J3 passed"
