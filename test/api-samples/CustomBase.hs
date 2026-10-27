{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import ExchangeAlgebra
import qualified ExchangeAlgebra.Accounting.Closing as Closing
import qualified ExchangeAlgebra.Accounting.Statements as St
import qualified ExchangeAlgebra.Accounting.TrialBalance as TB

-- | One extra coordinate on each posting.
data Department
    = Shop
    | Online
    | AnyDepartment
    deriving (Eq, Ord, Show, Generic)

instance Hashable Department

instance Element Department where
    wildcard = AnyDepartment

instance BaseClass Department

-- | The built-in title paired with a department.
type DepartmentBase = HatBase (AccountTitles, Department)

-- | A department-aware algebra entry.
type Entry = Alg MoneyDecimal DepartmentBase

-- | An entry over the built-in account titles only.
type TitleEntry = Alg MoneyDecimal (HatBase AccountTitles)

instance ExBaseClass DepartmentBase where
    getAccountTitle (_ :< (title, _)) = title
    setAccountTitle (hat :< (_, department)) title = hat :< (title, department)

-- | Two departments. The shop returns part of a purchase and makes a loss.
ledger :: Entry
ledger = 500 .@ Not :< (Cash, Shop)
      .+ 500 .@ Not :< (CapitalStock, Shop)
      .+ 400 .@ Not :< (Purchases, Shop)
      .+ 400 .@ Hat :< (Cash, Shop)
      .+  30 .@ Not :< (Cash, Shop)
      .+  30 .@ Hat :< (Purchases, Shop)
      .+ 200 .@ Not :< (Cash, Shop)
      .+ 200 .@ Not :< (Sales, Shop)
      .+  60 .@ Not :< (RentExpense, Shop)
      .+  60 .@ Hat :< (Cash, Shop)
      .+ 999 .@ Not :< (Cash, Online)
      .+ 999 .@ Not :< (Sales, Online)

-- | Statements receive one scope: select the shop and drop the department.
shopOnly :: Entry -> TitleEntry
shopOnly = mapMaybePosting pick
  where
    pick value (hat :< (title, Shop)) = Just (value, hat :< title)
    pick _ _ = Nothing

-- | Fail with both values when an expectation is not met.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check name expected actual
    | expected == actual = pure ()
    | otherwise = fail (name ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Side and amount of every presented line of one title.
linesOf :: AccountTitles -> St.FinancialStatements MoneyDecimal -> [(Side, MoneyDecimal)]
linesOf title statements =
    [ (St._lineSide line, St._lineAmount line)
    | line <- St._statementLines statements
    , St._lineAccount line == title
    ]

-- | Balances of the period-result subtotals.
periodResults :: St.FinancialStatements MoneyDecimal -> [TB.AccountBalance MoneyDecimal]
periodResults statements =
    [ St._statementSubtotalBalance subtotal
    | subtotal <- St._statementSubtotals statements
    , St._statementSubtotalMetric subtotal == St.PeriodResultMetric
    ]

-- | Present the shop, close it, and audit the deficit separately.
main :: IO ()
main = do
    let shop = shopOnly ledger
        context = St.jcciSecondGradeContext St.Standalone
    pl <- either (fail . show) pure (St.present context TB.BeforeClosing shop)
    -- Purchases 400 less the return 30.
    check "purchases net" [(Debit, 370)] (linesOf Purchases pl)
    -- Sales 200 - purchases 370 - rent 60 = a loss of 230.
    check "period result" [TB.DebitBalance 230] (periodResults pl)
    -- The online department's 999 is outside the selected scope.
    check "online excluded" [(Credit, 200)] (linesOf Sales pl)
    closing <- either (fail . show) pure (Closing.closingEntries shop)
    let closed = shop .+ closing
    -- The deficit does not stop the statements.
    bs <- either (fail . show) pure (St.present context TB.AfterClosing closed)
    check "deficit stays in equity" [(St.EquitySection, Debit, 230)]
        [ (St._lineSection line, St._lineSide line, St._lineAmount line)
        | line <- St._statementLines bs
        , St._lineAccount line == RetainedEarnings
        ]
    check "cash" [(Debit, 270)] (linesOf Cash bs)
    -- The audit is a separate function. The caller decides what to do with it.
    let audit = TB.trialBalanceFindings
            (TB.TrialBalanceInput closed TB.AfterClosing Map.empty [] Set.empty)
    check "deficit reported"
        [TB.UnexplainedAbnormalBalance RetainedEarnings Credit (TB.DebitBalance 230)]
        audit
