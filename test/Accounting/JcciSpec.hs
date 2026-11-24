-- | Check the JCCI vocabulary against the full chart and the public pattern declarations.
module Accounting.JcciSpec (runTests) where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Either (isLeft)
import Data.Hashable (hash, hashWithSalt)
import Data.List (groupBy, isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import ExchangeAlgebra.Accounting.Account (AccountTitles(..), AccountVocabulary(..))
import qualified ExchangeAlgebra.Accounting.Account.Jcci as J
import ExchangeAlgebra.Algebra.Base (Hat(..), HatBase(..), wildcard)
import ExchangeAlgebra.Algebra.Core (Alg, Redundant((.+)), (.@), mapBasePart, toList)
import ExchangeAlgebra.Algebra.Value (MoneyDecimal)
import Support (assertEqual, readFileStrict)

runTests :: IO ()
runTests = do
    testPatterns
    testNarrow
    testBinaryAndHash
    testVocabulary
    testNarrowAlg

-- | Enumerate the concrete titles accepted by the restricted vocabulary.
jcciTitles :: [J.JcciTitles]
jcciTitles = mapMaybe J.narrow [title | title <- [minBound .. maxBound], title /= AccountTitle]

-- | Pin the full chart's macroeconomic accounts independently of the row tags.
macroTitles :: [AccountTitles]
macroTitles =
    [ ReserveDepositReceivable
    , GovernmentService
    , ReserveForDepreciation
    , DepositPayable
    , ReserveDepositPayable
    , CentralBankNotePayable
    , ConsumptionExpenditure
    , SubsidyExpense
    , CentralBankPaymentExpense
    , ValueAdded
    , SubsidyIncome
    , WageEarned
    , TaxesRevenue
    , CentralBankPaymentIncome
    ]

-- | Compare the source declarations with the concrete titles in declaration order.
testPatterns :: IO ()
testPatterns = do
    source <- lines <$> readFileStrict "src/ExchangeAlgebra/Accounting/Account/Jcci.hs"
    definitions <- traverse patternName
        [block | block@(line : _) <- groupBy continues source, "pattern " `isPrefixOf` line]
    let exports = [name | [",", "pattern", name] <- map words source]
        complete = completeNames source
        expected = map (show . J.widen) jcciTitles ++ ["AnyJcci"]
    assertNames "JCCI pattern exports" expected exports
    assertNames "JCCI pattern definitions and constructor wiring" expected (concat definitions)
    assertNames "JCCI COMPLETE patterns" expected complete
  where
    continues _ line = case line of
        c : _ -> isSpace c
        [] -> False

-- | Compare both directions, then check declaration order and duplicates.
assertNames :: String -> [String] -> [String] -> IO ()
assertNames label expected actual = do
    let expectedSet = Set.fromList expected
        actualSet = Set.fromList actual
    assertEqual (label ++ " (missing, extra)") ([], [])
        ( Set.toAscList (expectedSet `Set.difference` actualSet)
        , Set.toAscList (actualSet `Set.difference` expectedSet)
        )
    assertEqual (label ++ " in declaration order") expected actual

-- | Read an account pattern and verify its exact constructor wiring.
patternName :: [String] -> IO [String]
patternName block = case (block, words (unwords block)) of
    (_, "pattern" : signature)
        | ("::" : ["JcciTitles"]) <- dropWhile (/= "::") signature
        , all (`notElem` signature) ["=", "<-"] -> pure []
    ([_], ["pattern", name, "=", "JcciTitles", target])
        | matches name target -> pure [name]
    ([header, body], ["pattern", name, "=", "JcciTitles", target])
        | words header == ["pattern", name, "="]
        , "    JcciTitles " `isPrefixOf` body
        , matches name target -> pure [name]
    _ -> fail ("Malformed or miswired JCCI pattern: " ++ unwords block)
  where
    matches "AnyJcci" target = target == "T.AccountTitle"
    matches name target = target == "T." ++ name

-- | Read all names in the multiline completeness pragma.
completeNames :: [String] -> [String]
completeNames source = case dropWhile (not . isPrefixOf "{-# COMPLETE") source of
    [] -> []
    header : rest ->
        let pragma = unwords (header : takeWhile (not . isPrefixOf "    #-}") rest)
            tokens = words [if c == ',' then ' ' else c | c <- pragma]
        in takeWhile (/= "::") (drop 2 tokens)

-- | Check that the embedding is inverse on every accepted title.
testNarrow :: IO ()
testNarrow = do
    let titles = [minBound .. maxBound]
        expected t
            | t `elem` macroTitles = Nothing
            | otherwise = Just t
    assertEqual "JCCI narrowing on the full chart"
        (map expected titles) (map (fmap J.widen . J.narrow) titles)
    assertEqual "JCCI wildcard" (Just J.AnyJcci) (J.narrow AccountTitle)
    assertEqual "JCCI Element wildcard" J.AnyJcci wildcard
    let accepted = jcciTitles ++ [J.AnyJcci]
    assertEqual "JCCI title spellings" (map (show . J.widen) accepted) (map show accepted)

-- | Check matching wire codes and hashes and reject codes outside the restricted chart.
testBinaryAndHash :: IO ()
testBinaryAndHash = do
    let titles = jcciTitles ++ [J.AnyJcci]
        fullTitles = map J.widen titles
        bytes = map Binary.encode fullTitles
        invalidBytes = BL.pack [0x80, 0x23] : map Binary.encode macroTitles
    assertEqual "JCCI Binary codes match AccountTitles" bytes (map Binary.encode titles)
    assertEqual "JCCI Binary round trips" (map Right titles) (map decodeTitle bytes)
    assertEqual "JCCI Binary rejects macroeconomic and retired codes"
        (replicate (length invalidBytes) True) (map (isLeft . decodeTitle) invalidBytes)
    assertEqual "JCCI hashes match AccountTitles" (map hash fullTitles) (map hash titles)
    assertEqual "JCCI salted hashes match AccountTitles"
        (map (hashWithSalt 42) fullTitles) (map (hashWithSalt 42) titles)
  where
    decodeTitle :: BL.ByteString -> Either String J.JcciTitles
    decodeTitle bytes = case Binary.decodeOrFail bytes of
        Left (_, _, message) -> Left message
        Right (_, _, title) -> Right title

-- | Compare title metadata and accounting checks supplied by the two instances.
testVocabulary :: IO ()
testVocabulary = do
    let fullTitles = map J.widen jcciTitles
    assertEqual "JCCI account specifications" (map vocabSpec fullTitles) (map vocabSpec jcciTitles)
    assertEqual "JCCI account semantics"
        (map vocabSemantics fullTitles) (map vocabSemantics jcciTitles)
    assertEqual "JCCI account divisions"
        (map vocabDivision fullTitles) (map vocabDivision jcciTitles)
    assertEqual "JCCI contra accounts" (map vocabContra fullTitles) (map vocabContra jcciTitles)
    assertEqual "JCCI impossible balance sides" vocabImpossibleBalanceSides
        [(J.widen title, side) | (title, side) <- vocabImpossibleBalanceSides]
    assertEqual "JCCI suspense titles" vocabSuspenseTitles (map J.widen vocabSuspenseTitles)
    assertEqual "JCCI additional closing residual titles" vocabAdditionalClosingResidualTitles
        (map J.widen vocabAdditionalClosingResidualTitles)
    assertEqual "JCCI reciprocal pairs" vocabReciprocalPairs
        [(J.widen left, J.widen right) | (left, right) <- vocabReciprocalPairs]
    assertEqual "JCCI closing target" closingTarget (J.widen closingTarget)

-- | Check rejection and ordered term preservation on both sides of a base.
testNarrowAlg :: IO ()
testNarrowAlg = do
    let ledger :: Alg MoneyDecimal (HatBase AccountTitles)
        ledger = 11 .@ Not :< Cash
              .+ 7 .@ Not :< Cash
              .+ 23 .@ Not :< Cash
              .+ 3 .@ Hat :< Cash
              .+ 5 .@ Hat :< Cash
              .+ 2 .@ Not :< AccountTitle
        mixed = ledger .+ 2 .@ Hat :< GovernmentService
                       .+ 3 .@ Not :< ReserveDepositReceivable
                       .+ 4 .@ Not :< GovernmentService
    assertEqual "narrowAlg rejects distinct macroeconomic titles in order"
        (Left [ReserveDepositReceivable, GovernmentService]) (J.narrowAlg mixed)
    narrowed <- either (fail . show) pure (J.narrowAlg ledger)
    assertEqual "narrowAlg retains every term" (length (toList ledger)) (length (toList narrowed))
    assertEqual "narrowAlg preserves term values, directions, and per-base order"
        ledger (mapBasePart J.widen narrowed)
