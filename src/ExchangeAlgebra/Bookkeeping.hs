{- |
    Module     : ExchangeAlgebra.Bookkeeping
    Copyright  : (c) Kaya Akagi. 2018-2026
    Maintainer : yakagika@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hiroshi Deguchi.

    Exchange Algebra is an algebraic description of bookkeeping systems.
    Details are below.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    == Closing-adjustment entry builders (決算整理仕訳)

    This module provides /posting builders/ (journal-entry templates) for the
    closing adjustments taught at the elementary-bookkeeping (日商簿記 3 級) level:
    cost of goods sold under the periodic (3-account) method, depreciation, the
    allowance for doubtful accounts, deferral\/accrual (経過勘定) entries, and the
    settlement of consumption\/corporate income tax.

    Unlike "ExchangeAlgebra.Algebra.Transfer" (which /relabels existing ledger
    balances/), most closing adjustments record /new postings whose amounts come
    from outside the ledger/ (period-end inventory, estimated allowance,
    depreciation expense, …). Each builder here therefore returns a fresh,
    balanced @'Alg' v b@ value.

    === Design

    * Every builder is constructed exclusively with the smart constructor
      @('.@')@, so a zero amount normalises to @Zero@ and a negative\/non-finite
      amount is rejected by @('.@')@'s @error@ path. No implicit @bar@\/@compress@
      is performed — builders only /generate/ new postings.

    * The base polymorphism is absorbed by an injection function @'MkBase'@ that
      the caller supplies once (see below).

    * The Hat\/Not label of each line follows the /accounting meaning/ of that
      line: an account that /increases/ uses @Not@ (so it lands on its home side),
      an account that /decreases/ uses @Hat@ (the opposite side). 'whichSide'
      then places each line on the correct debit\/credit side. Consequently every
      builder is debit-credit balanced: @'norm' ('decL' x) == 'norm' ('decR' x)@.
-}

{-# LANGUAGE FlexibleContexts   #-}

module ExchangeAlgebra.Bookkeeping
    ( -- * Base injection
      MkBase
      -- * Cost of goods sold (売上原価, 3 分法)
    , cogsAdjustmentEntries
      -- * Depreciation (減価償却)
    , depreciationIndirectEntry
    , depreciationDirectEntry
      -- * Allowance for doubtful accounts (貸倒引当金)
    , allowanceReplenishmentEntry
    , allowanceResetEntries
      -- * Deferral / accrual (経過勘定)
    , prepaidExpenseEntry
    , unearnedRevenueEntry
    , accruedRevenueEntry
    , accruedExpenseEntry
    , reversingEntry
      -- * Tax settlement (消費税・法人税等)
    , consumptionTaxSettlementEntry
    , corporateTaxInterimEntry
    , corporateTaxSettlementEntries
      -- * Equity method (持分法)
    , equityMethodEarningsEntry
    , equityMethodDividendEntry
    , equityMethodEntries
    , equityMethodBalance
      -- * Prior-period error correction (前期修正)
    , priorPeriodErrorCorrection
    ) where

import           ExchangeAlgebra.Algebra
-- @MoneyDecimal@ is used only by the Haddock doctest examples (which run in this
-- module's import scope), not the module body; keep it imported for the doctests.
import           ExchangeAlgebra.Value    (MoneyDecimal)
import           GHC.Stack (HasCallStack)

-- | Injection that tells a builder how to wrap a @(v'Hat', 'AccountTitles')@ pair
-- into the concrete base @b@ in use. The caller supplies it once.
--
-- @
-- -- single-axis base (account title only):
-- mk1 :: MkBase (HatBase AccountTitles)
-- mk1 = (':<')
--
-- -- multi-axis base, filling the extra axes with fixed values:
-- mk4 :: MkBase (HatBase (AccountTitles, Name, CountUnit, Subject))
-- mk4 h t = h :< (t, \"\", Yen, \"\")
-- @
type MkBase b = Hat -> AccountTitles -> b

-- | (借) debit-meaning convenience: the account /increases/, so it carries 'Not'
-- and lands on its home side.
{-# INLINE up #-}
up :: (HatVal v, ExBaseClass b) => MkBase b -> v -> AccountTitles -> Alg v b
up mk v t = v .@ mk Not t

-- | The account /decreases/, so it carries v'Hat' (the opposite of its home side).
{-# INLINE down #-}
down :: (HatVal v, ExBaseClass b) => MkBase b -> v -> AccountTitles -> Alg v b
down mk v t = v .@ mk Hat t

------------------------------------------------------------------
-- * Cost of goods sold (売上原価, 3 分法)
------------------------------------------------------------------

-- | Cost-of-goods-sold adjustment under the periodic\/3-account method
-- (3 分法; lecture ch.10, 24), the four "しいくりくりしい" postings:
--
-- > (借) 仕入       beg   (貸) 繰越商品   beg   -- opening inventory
-- > (借) 繰越商品   end   (貸) 仕入       end   -- closing inventory
--
-- After this entry the @Purchases@ balance equals cost of goods sold and the
-- @MerchandiseInventory@ balance equals the closing inventory.
--
-- The result is debit-credit balanced (each side sums to @beg + end@):
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = cogsAdjustmentEntries mk 100 50 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
-- >>> norm (decL e)
-- 150
--
-- Complexity: O(1)
cogsAdjustmentEntries :: (HatVal v, ExBaseClass b)
                      => MkBase b  -- ^ base injection
                      -> v         -- ^ beginning inventory (期首商品棚卸高)
                      -> v         -- ^ ending inventory (期末商品棚卸高)
                      -> Alg v b
cogsAdjustmentEntries mk beg end =
       up   mk beg Purchases             -- (借) 仕入       期首
    .+ down mk beg MerchandiseInventory  -- (貸) 繰越商品   期首
    .+ up   mk end MerchandiseInventory  -- (借) 繰越商品   期末
    .+ down mk end Purchases             -- (貸) 仕入       期末

------------------------------------------------------------------
-- * Depreciation (減価償却)
------------------------------------------------------------------

-- | Indirect-method depreciation (間接法; lecture ch.18):
--
-- > (借) 減価償却費   amt   (貸) 減価償却累計額   amt
--
-- 'AccumulatedDepreciation' is a credit-balance valuation account (評価勘定).
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = depreciationIndirectEntry mk 150 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
--
-- Complexity: O(1)
depreciationIndirectEntry :: (HatVal v, ExBaseClass b)
                          => MkBase b -> v -> Alg v b
depreciationIndirectEntry mk amt =
       up mk amt Depreciation              -- (借) 減価償却費
    .+ up mk amt AccumulatedDepreciation   -- (貸) 減価償却累計額

-- | Direct-method depreciation (直接法; lecture ch.18):
--
-- > (借) 減価償却費   amt   (貸) <asset>   amt
--
-- The asset's carrying amount is reduced directly. @assetTitle@ is the fixed
-- asset being depreciated (e.g. 'Fixtures', 'Building', 'Vehicle').
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = depreciationDirectEntry mk 90 Fixtures :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
--
-- Complexity: O(1)
depreciationDirectEntry :: (HatVal v, ExBaseClass b)
                        => MkBase b
                        -> v            -- ^ depreciation amount
                        -> AccountTitles  -- ^ asset being depreciated
                        -> Alg v b
depreciationDirectEntry mk amt assetTitle =
       up   mk amt Depreciation   -- (借) 減価償却費
    .+ down mk amt assetTitle     -- (貸) <asset>

------------------------------------------------------------------
-- * Allowance for doubtful accounts (貸倒引当金)
------------------------------------------------------------------

-- | Allowance for doubtful accounts under the /差額補充法/ (replenishment;
-- lecture ch.16). Only the difference between the estimate and the current
-- balance is recorded:
--
-- * @estimate > current@: provide the shortfall —
--   @(借) 貸倒引当金繰入 / (貸) 貸倒引当金@.
-- * @estimate < current@: release the excess —
--   @(借) 貸倒引当金 / (貸) 貸倒引当金戻入@ (uses 'ReversalOfAllowanceForDoubtfulAccounts').
-- * @estimate == current@: no entry (@Zero@).
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = allowanceReplenishmentEntry mk 1400 1000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
-- >>> norm (decL e)
-- 400
-- >>> allowanceReplenishmentEntry mk 2000 2000 == (Zero :: Alg MoneyDecimal (HatBase AccountTitles))
-- True
--
-- Complexity: O(1)
allowanceReplenishmentEntry :: (HatVal v, ExBaseClass b)
                            => MkBase b
                            -> v   -- ^ estimated allowance (当期末貸倒見積額)
                            -> v   -- ^ current allowance balance (貸倒引当金残高)
                            -> Alg v b
allowanceReplenishmentEntry mk estimate current
    | estimate >= current =
        let diff = estimate - current
        in    up mk diff ProvisionForDoubtfulAccounts   -- (借) 貸倒引当金繰入
           .+ up mk diff AllowanceForDoubtfulAccounts    -- (貸) 貸倒引当金
    | otherwise =
        let diff = current - estimate
        in    down mk diff AllowanceForDoubtfulAccounts            -- (借) 貸倒引当金
           .+ up   mk diff ReversalOfAllowanceForDoubtfulAccounts  -- (貸) 貸倒引当金戻入

-- | Allowance for doubtful accounts under the /洗替法/ (reset; lecture ch.16):
-- release the whole current balance, then provide the whole estimate.
--
-- > (借) 貸倒引当金     current   (貸) 貸倒引当金戻入   current
-- > (借) 貸倒引当金繰入 estimate  (貸) 貸倒引当金       estimate
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = allowanceResetEntries mk 2400 1000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
-- >>> norm (decL e)
-- 3400
--
-- Complexity: O(1)
allowanceResetEntries :: (HatVal v, ExBaseClass b)
                      => MkBase b
                      -> v   -- ^ estimated allowance for this period
                      -> v   -- ^ current allowance balance to release
                      -> Alg v b
allowanceResetEntries mk estimate current =
       down mk current AllowanceForDoubtfulAccounts            -- (借) 貸倒引当金
    .+ up   mk current ReversalOfAllowanceForDoubtfulAccounts  -- (貸) 貸倒引当金戻入
    .+ up   mk estimate ProvisionForDoubtfulAccounts           -- (借) 貸倒引当金繰入
    .+ up   mk estimate AllowanceForDoubtfulAccounts           -- (貸) 貸倒引当金

------------------------------------------------------------------
-- * Deferral / accrual (経過勘定; lecture ch.21)
------------------------------------------------------------------

-- | Deferral of a prepaid expense (費用の前払; 前払費用):
--
-- > (借) 前払費用   amt   (貸) <expense>   amt
--
-- Moves the unexpired portion of @expenseTitle@ out to 'PrepaidExpenses'. The
-- opening reversing entry of the next period is @'reversingEntry'@ of this.
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = prepaidExpenseEntry mk 4000 RentExpense :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
--
-- Complexity: O(1)
prepaidExpenseEntry :: (HatVal v, ExBaseClass b)
                    => MkBase b
                    -> v            -- ^ prepaid amount
                    -> AccountTitles  -- ^ expense being deferred
                    -> Alg v b
prepaidExpenseEntry mk amt expenseTitle =
       up   mk amt PrepaidExpenses   -- (借) 前払費用
    .+ down mk amt expenseTitle      -- (貸) <expense>

-- | Deferral of unearned revenue (収益の前受; 前受収益):
--
-- > (借) <revenue>   amt   (貸) 前受収益   amt
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = unearnedRevenueEntry mk 4000 RentalIncome :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
--
-- Complexity: O(1)
unearnedRevenueEntry :: (HatVal v, ExBaseClass b)
                     => MkBase b
                     -> v            -- ^ unearned amount
                     -> AccountTitles  -- ^ revenue being deferred
                     -> Alg v b
unearnedRevenueEntry mk amt revenueTitle =
       down mk amt revenueTitle    -- (借) <revenue>
    .+ up   mk amt UnearnedRevenue  -- (貸) 前受収益

-- | Accrual of accrued revenue (収益の未収; 未収収益):
--
-- > (借) 未収収益   amt   (貸) <revenue>   amt
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = accruedRevenueEntry mk 2500 InterestEarned :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
--
-- Complexity: O(1)
accruedRevenueEntry :: (HatVal v, ExBaseClass b)
                    => MkBase b
                    -> v            -- ^ accrued amount
                    -> AccountTitles  -- ^ revenue being accrued
                    -> Alg v b
accruedRevenueEntry mk amt revenueTitle =
       up mk amt AccruedRevenue  -- (借) 未収収益
    .+ up mk amt revenueTitle    -- (貸) <revenue>

-- | Accrual of an accrued expense (費用の未払; 未払費用):
--
-- > (借) <expense>   amt   (貸) 未払費用   amt
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = accruedExpenseEntry mk 2500 InterestExpense :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
--
-- Complexity: O(1)
accruedExpenseEntry :: (HatVal v, ExBaseClass b)
                    => MkBase b
                    -> v            -- ^ accrued amount
                    -> AccountTitles  -- ^ expense being accrued
                    -> Alg v b
accruedExpenseEntry mk amt expenseTitle =
       up mk amt expenseTitle    -- (借) <expense>
    .+ up mk amt AccruedExpenses  -- (貸) 未払費用

-- | Reversing\/cancelling entry, a vocabulary alias for the Hat operation
-- @('.^')@ (lecture ch.20-21, 再振替仕訳\/訂正仕訳). This is the direct
-- application of /Hat involution/: flipping Hat\/Not on every posting turns an
-- entry into its exact reversal.
--
-- * Opening reversal (再振替仕訳): @reversingEntry deferral@ at the start of the
--   next period undoes a deferral\/accrual entry.
--
-- * Correction (訂正仕訳): a mistaken entry @wrong@ is corrected by
--   @reversingEntry wrong '.+' correct@. Because @('.+')@ keeps same-base
--   postings as an ordered /sequence/ (the redundancy), the wrong entry and its
--   reversal both remain in the algebra as an /audit trail/ of the correction;
--   @'bar'@ then nets them out to the corrected balance.
--
-- @reversingEntry@ is an involution (@reversingEntry . reversingEntry = id@) and
-- @'bar' (x '.+' reversingEntry x) = Zero@ (an entry plus its reversal cancels).
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let x = accruedExpenseEntry mk 2500 InterestExpense :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> reversingEntry (reversingEntry x) == x
-- True
-- >>> bar (x .+ reversingEntry x) == Zero
-- True
--
-- Complexity: O(1) for a singleton, O(n) for an
-- 'ExchangeAlgebra.Algebra.Internal.Liner' (n base keys).
{-# INLINE reversingEntry #-}
reversingEntry :: (Redundant a v b) => a v b -> a v b
reversingEntry = (.^)

------------------------------------------------------------------
-- * Tax settlement (消費税・法人税等; lecture ch.23)
------------------------------------------------------------------

-- | Consumption-tax settlement at closing (税抜処理方式; lecture ch.23): offset
-- consumption tax received against consumption tax paid and book the unpaid
-- balance.
--
-- > (借) 仮受消費税   received   (貸) 仮払消費税   paid
-- >                              (貸) 未払消費税   received - paid
--
-- __Out of 3-級 scope:__ @received < paid@ (a tax refund, 還付) is rejected with
-- @error@. At the elementary level the consumption tax received always exceeds
-- the consumption tax paid.
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = consumptionTaxSettlementEntry mk 1000 20000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
-- >>> norm (projByAccountTitle AccruedConsumptionTax e)
-- 19000
--
-- Complexity: O(1)
consumptionTaxSettlementEntry :: (HasCallStack, HatVal v, ExBaseClass b)
                              => MkBase b
                              -> v   -- ^ consumption tax paid (仮払消費税)
                              -> v   -- ^ consumption tax received (仮受消費税)
                              -> Alg v b
consumptionTaxSettlementEntry mk paid received
    | received < paid =
        error $ "consumptionTaxSettlementEntry: received (" ++ show received
             ++ ") < paid (" ++ show paid
             ++ "); a consumption-tax refund is out of 日商簿記 3 級 scope."
    | otherwise =
        let unpaid = received - paid
        in    down mk received ConsumptionTaxReceived  -- (借) 仮受消費税
           .+ down mk paid     ConsumptionTaxPaid       -- (貸) 仮払消費税
           .+ up   mk unpaid   AccruedConsumptionTax    -- (貸) 未払消費税

-- | Interim (mid-year) payment of corporate income tax (中間納付; lecture ch.23):
--
-- > (借) 仮払法人税等   amt   (貸) 現金   amt
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = corporateTaxInterimEntry mk 500000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
--
-- Complexity: O(1)
corporateTaxInterimEntry :: (HatVal v, ExBaseClass b)
                         => MkBase b -> v -> Alg v b
corporateTaxInterimEntry mk amt =
       up   mk amt PrepaidCorporateIncomeTaxes  -- (借) 仮払法人税等
    .+ down mk amt Cash                          -- (貸) 現金

-- | Corporate income tax settlement at closing (lecture ch.23): book the total
-- tax expense, credit the interim payment already made, and recognise the
-- unpaid balance.
--
-- > (借) 法人税等   total   (貸) 仮払法人税等   interim
-- >                         (貸) 未払法人税等   total - interim
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = corporateTaxSettlementEntries mk 800000 500000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
-- >>> norm (projByAccountTitle AccruedCorporateIncomeTaxes e)
-- 300000
--
-- Complexity: O(1)
corporateTaxSettlementEntries :: (HasCallStack, HatVal v, ExBaseClass b)
                              => MkBase b
                              -> v   -- ^ total corporate income tax (法人税等)
                              -> v   -- ^ interim payment already made (仮払法人税等)
                              -> Alg v b
corporateTaxSettlementEntries mk total interim
    -- Same guard style as 'consumptionTaxSettlementEntry': an over-prepayment
    -- (refund position) is out of 3-級 scope, so reject it with a clear error
    -- instead of letting the negative @unpaid@ hit @('.@')@'s generic one.
    | interim > total =
        error $ "corporateTaxSettlementEntries: interim (" ++ show interim
             ++ ") > total (" ++ show total
             ++ "); a corporate-tax refund (over-prepayment) is out of 日商簿記 3 級 scope."
    | otherwise =
        let unpaid = total - interim
        in    up   mk total   CorporateIncomeTaxes          -- (借) 法人税等
           .+ down mk interim PrepaidCorporateIncomeTaxes    -- (貸) 仮払法人税等
           .+ up   mk unpaid  AccruedCorporateIncomeTaxes    -- (貸) 未払法人税等

------------------------------------------------------------------
-- * Equity method (持分法; lecture T4b)
------------------------------------------------------------------

-- | Equity-method earnings accrual (持分法による投資利益の計上):
--
-- > (借) 関係会社株式   share   (貸) 持分法による投資利益   share
--
-- @share@ is the investor's proportionate share of the investee's net income
-- (= investee NI × ownership %). The investment carrying amount increases by
-- @share@.
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = equityMethodEarningsEntry mk 438000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
-- >>> norm (decL e)
-- 438000
--
-- Complexity: O(1)
equityMethodEarningsEntry :: (HatVal v, ExBaseClass b)
                          => MkBase b
                          -> v         -- ^ investor's share of investee NI (持分利益)
                          -> Alg v b
equityMethodEarningsEntry mk share =
       up mk share InvestmentInAssociate       -- (借) 関係会社株式
    .+ up mk share EquityInEarningsOfInvestee  -- (貸) 持分法による投資利益

-- | Equity-method dividend received (受取配当による投資簿価の減額):
--
-- > (借) 現金   div   (貸) 関係会社株式   div
--
-- Under the equity method a dividend received from the investee is /not/ income;
-- it reduces the carrying amount of the investment.
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = equityMethodDividendEntry mk 800000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
-- >>> norm (decL e)
-- 800000
--
-- Complexity: O(1)
equityMethodDividendEntry :: (HatVal v, ExBaseClass b)
                          => MkBase b
                          -> v         -- ^ dividend received (受取配当金)
                          -> Alg v b
equityMethodDividendEntry mk div' =
       up   mk div' Cash                   -- (借) 現金
    .+ down mk div' InvestmentInAssociate  -- (貸) 関係会社株式

-- | Combined equity-method closing entries: record the earnings accrual
-- then the dividend reduction.
--
-- > (借) 関係会社株式   share   (貸) 持分法による投資利益   share
-- > (借) 現金           div     (貸) 関係会社株式           div
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = equityMethodEntries mk 438000 800000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
--
-- Complexity: O(1)
equityMethodEntries :: (HatVal v, ExBaseClass b)
                    => MkBase b
                    -> v         -- ^ investor's share of investee NI (持分利益)
                    -> v         -- ^ dividend received (受取配当金)
                    -> Alg v b
equityMethodEntries mk share div' =
       equityMethodEarningsEntry mk share
    .+ equityMethodDividendEntry mk div'

-- | Equity-method carrying-amount balance (関係会社株式の簿価残高).
--
-- Projects all @'InvestmentInAssociate'@ postings from an accumulated @'Alg'@
-- and returns their @'norm'@ (= net carrying amount). The caller accumulates
-- the initial acquisition entry plus any 'equityMethodEarningsEntry' and
-- 'equityMethodDividendEntry' calls; this function reads the result without
-- recomputing it by hand — /correct-by-construction/.
--
-- Example (#18 anchor doctest):
-- acquisition cost 2,400,000 + share of NI 0.30 × 1,460,000 (= 438,000)
-- − dividend received 800,000 = carrying amount 2,038,000.
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> -- initial acquisition: Dr 関係会社株式 2,400,000 / Cr Cash 2,400,000
-- >>> let acq = up mk 2400000 InvestmentInAssociate .+ down mk 2400000 Cash :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> let entries = equityMethodEntries mk 438000 800000 :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> let ledger = acq .+ entries
-- >>> equityMethodBalance ledger
-- 2038000
--
-- Complexity: O(s) (s = number of scalar entries in the accumulated algebra)
equityMethodBalance :: (HatVal v, ExBaseClass b) => Alg v b -> v
equityMethodBalance = norm . bar . projByAccountTitle InvestmentInAssociate

------------------------------------------------------------------
-- * Prior-period error correction (前期修正)
------------------------------------------------------------------

-- | Prior-period error correction (前期修正・誤謬訂正; #15 Briggs anchor).
--
-- Records the correction of an error discovered in a prior period:
-- the current-period portion goes to an expense account, while the
-- prior-period portion bypasses the income statement and is charged
-- directly to 'RetainedEarnings' (繰越利益剰余金) — conforming to
-- IAS 8 \/ ASC 250-10 retrospective-restatement treatment.
--
-- > (借) \<expense\>         current   (貸) \<asset\>   current + prior
-- > (借) RetainedEarnings   prior
--
-- Structural guarantee (thesis property): the prior-period amount is
-- /always/ routed to 'RetainedEarnings'; the caller cannot accidentally
-- book it to an income-statement account — the parameter type forces the
-- split.
--
-- 'RetainedEarnings' is a credit-balance equity account (@home = Credit@),
-- so a reduction (debit-side charge) is recorded with @down@ (@Hat@),
-- which places it on the debit side — correct-by-construction.
--
-- The credit leg (asset reduction) is collapsed into one posting
-- @down mk (current + prior) assetAcc@ because @'Num' v@ is available
-- from the @'HatVal'@ superclass, making value addition clean; the
-- two-line @.+@ alternative is equally valid under the seq-redundancy
-- principle but the single line is more readable here.
--
-- === Example (#15 anchor: patent 55,000 \/ 10 yr, discovered in 2028)
--
-- * Patent cost 55,000, useful life 10 years → annual amortisation 5,500.
-- * Error discovered in 2028: 2 prior years (2026, 2027) were not amortised.
-- * Current-year amortisation 5,500 → expense (@AmortizationExpense@).
-- * Prior 2 years 11,000 → directly to @RetainedEarnings@ (前期修正).
-- * Credit @Patent@ 16,500 total.
--
-- >>> let mk = (:<) :: MkBase (HatBase AccountTitles)
-- >>> let e = priorPeriodErrorCorrection mk 5500 11000 AmortizationExpense Patent :: Alg MoneyDecimal (HatBase AccountTitles)
-- >>> norm (decL e) == norm (decR e)
-- True
-- >>> norm (decR e)
-- 16500
-- >>> norm (decL e)
-- 16500
--
-- Complexity: O(1)
priorPeriodErrorCorrection :: (HatVal v, ExBaseClass b)
                           => MkBase b
                           -> v              -- ^ current-period portion (当期費用; → @expenseAcc@)
                           -> v              -- ^ prior-period portion   (前期修正; → 'RetainedEarnings')
                           -> AccountTitles  -- ^ current-period expense account (e.g. 'AmortizationExpense')
                           -> AccountTitles  -- ^ affected asset credited (e.g. 'Patent')
                           -> Alg v b
priorPeriodErrorCorrection mk current prior expenseAcc assetAcc =
       up   mk current expenseAcc              -- (借) 当期費用 (expense 増, home=Debit → Not)
    .+ down mk prior   RetainedEarnings        -- (借) 繰越利益剰余金 (equity 減, home=Credit → Hat=debit側)
    .+ down mk (current + prior) assetAcc      -- (貸) 資産 (合計 current+prior 分だけ減額)
