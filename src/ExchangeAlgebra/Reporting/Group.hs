{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

{- |
Module      : ExchangeAlgebra.Reporting.Group
Description : Presentation groups and the contra netting policy (Definition 7, Land 3).

A /presentation group/ is a statement block made of one or more __gross__
account titles and the __contra__ (評価勘定) titles that are deducted from
them, rendered as @gross lines → deduction lines → net line@. It is the
mechanism the Definition 7 contra amendment left open: Land 2 fixed the
classification (contra assets are @Assets@ with @isContra = True@) but kept a
display-compatibility shim so that the contra accounts still /appeared/ in the
liability column; this module supplies the real deduction presentation and
lets that shim be removed.

This module is deliberately free of both the CSV writers and the validated
reporting pipeline: it is the __single implementation__ of grouping and
netting, and "ExchangeAlgebra.Write" ('ExchangeAlgebra.Write.bsRows',
'ExchangeAlgebra.Write.plRows') is its consumer. Placing it here rather than
inside "ExchangeAlgebra.Reporting.Presentation" is forced by the type of the
legacy writers: they are total, base-polymorphic functions
(@ExBaseClass b => Alg n b -> [[Text]]@), whereas @present@ is specialised to
@HatBase AccountTitles@, is gated by an opaque
@ValidatedTrialBalance@, and answers in @Either@. Routing @bsRows@ through
@present@ would therefore change it from a pure formatter into a partial,
validation-gated one; duplicating the netting rule in both places is exactly
what this module exists to prevent.

=== Value domain

Statement amounts are held as a @(debit gross, credit gross)@ pair and only
netted at the last moment, into a 'RelativeAmount' carrying a __non-negative__
magnitude plus a direction flag. Stored and rendered magnitudes therefore stay
in \(\mathbb{R}_0^+\), including for soft-invariant value types such as
@MoneyDecimal@. A minus sign exists only in rendered text, never in a stored
'RelativeAmount'.

=== Block invariant

Within one block the displayed amounts sum to the net line:

> Σ (gross rows) + Σ (subgroup rows) + Σ (deduction rows) = net row

A deduction row is stated /relative to the group's normal side/, so an
ordinary contra balance is already negative there and the sum above is a plain
addition, not a subtraction. A contra that exceeds its gross members simply
makes the net row negative (that is the @contra > parent@ edge case); a group
whose gross members are absent entirely nets to the negated deduction (the
@parent absent@ edge case). Both are handled by the same arithmetic.
-}
module ExchangeAlgebra.Reporting.Group
    ( -- * Groups
      PresentationGroup(..)
    , PresentationGroupDef(..)
    , defaultPresentationGrouping
    , presentationGroupOf
    , lookupGroupDef
    , groupNormalSide
    , groupingForDivisions
      -- * Amounts
    , RelativeAmount(..)
    , relativeTo
    , addGross
      -- * Grouped rows
    , GroupRowKind(..)
    , GroupRow(..)
    , GroupedPresentation(..)
    , presentGroups
    ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Text (Text)

import           ExchangeAlgebra.Algebra (HatVal(zeroValue), nearlyEqScaled)
import           ExchangeAlgebra.Algebra.Base
                     ( AccountDivision(..)
                     , AccountTitles(..)
                     , Side(..)
                     , defaultSide
                     )

------------------------------------------------------------------
-- Groups
------------------------------------------------------------------

-- | Identity of a statement presentation block. These are reporting
-- identities, never posting coordinates: they name a group of account titles,
-- not an account.
data PresentationGroup
    = TradeReceivablesGroup   -- ^ 売上債権 − 貸倒引当金
    | DepreciableAssetsGroup  -- ^ 償却性有形固定資産 − 減価償却累計額
    | NetSalesGroup           -- ^ 総売上高 − 売上割戻
    | NetPurchasesGroup       -- ^ 総仕入高 − 仕入割戻
    | IncomeTaxesGroup        -- ^ 法人税等 − 還付法人税等
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | One block of a statement: the gross titles, the contra titles deducted
-- from them, and (optionally) the group this block rolls up into.
--
-- @pgGross@ and @pgDeductions@ are ordered; rows are emitted in that order so
-- that the output is deterministic. Every @pgDeductions@ member is expected to
-- be a registry contra account of the same division as @pgDivision@, and no
-- @pgGross@ member is; the test-suite pins both, together with the disjointness
-- of the membership sets and the fact that every registry contra account
-- belongs to exactly one group.
data PresentationGroupDef = PresentationGroupDef
    { pgKey        :: PresentationGroup
    , pgDivision   :: AccountDivision
      -- ^ Statement column the block belongs to. Fixes the group's normal
      -- side via 'groupNormalSide'.
    , pgLabel      :: Text
      -- ^ Label of the net line (and of the carried subtotal when this block
      -- rolls up into another one).
    , pgGross      :: [AccountTitles]
    , pgDeductions :: [AccountTitles]
    , pgParent     :: Maybe PresentationGroup
      -- ^ When set, this block's net is carried into the named block as a
      -- 'SubgroupRow' and only the outermost block contributes to the column
      -- total. The default grouping is flat; nesting exists so that a caller
      -- can state a multi-level statement without a second netting rule.
    }
    deriving (Show, Eq)

-- | Normal (deduction-free) side of a group, i.e. the side its net line is
-- stated against.
groupNormalSide :: PresentationGroupDef -> Side
groupNormalSide = defaultSide . pgDivision

-- | The grouping applied by 'ExchangeAlgebra.Write.bsRows' and
-- 'ExchangeAlgebra.Write.plRows' unless a caller supplies its own.
--
-- The membership lists are presentation policy, not algebra: they say which
-- gross titles a single undifferentiated valuation account is deducted from
-- under JGAAP's indirect method (間接控除法). The registry keeps exactly five
-- contra accounts, so there are exactly five blocks. All blocks are flat —
-- a nested default would assert a subtotal structure that the chart does not
-- yet carry.
defaultPresentationGrouping :: [PresentationGroupDef]
defaultPresentationGrouping =
    [ PresentationGroupDef
        { pgKey = TradeReceivablesGroup
        , pgDivision = Assets
        , pgLabel = "TradeReceivablesNet"
        , pgGross =
            [ NotesReceivable
            , AccountsReceivable
            , ElectronicallyRecordedReceivable
            , CreditCardReceivable
            ]
        , pgDeductions = [AllowanceForDoubtfulAccounts]
        , pgParent = Nothing
        }
    , PresentationGroupDef
        { pgKey = DepreciableAssetsGroup
        , pgDivision = Assets
        , pgLabel = "DepreciableAssetsNet"
        , pgGross =
            [ Building
            , Structures
            , Machinery
            , Vehicle
            , ToolsAndInstruments
            , Fixtures
            ]
        , pgDeductions = [AccumulatedDepreciation]
        , pgParent = Nothing
        }
    , PresentationGroupDef
        { pgKey = NetSalesGroup
        , pgDivision = Revenue
        , pgLabel = "NetSales"
        , pgGross = [Sales]
        , pgDeductions = [SalesRebates]
        , pgParent = Nothing
        }
    , PresentationGroupDef
        { pgKey = NetPurchasesGroup
        , pgDivision = Cost
        , pgLabel = "NetPurchases"
        , pgGross = [Purchases]
        , pgDeductions = [PurchaseRebates]
        , pgParent = Nothing
        }
    , PresentationGroupDef
        { pgKey = IncomeTaxesGroup
        , pgDivision = Cost
        , pgLabel = "IncomeTaxesNet"
        , pgGross = [CorporateIncomeTaxes, AdditionalIncomeTaxesForPriorPeriods]
        , pgDeductions = [RefundOfIncomeTaxes]
        , pgParent = Nothing
        }
    ]

-- | Block a title belongs to under 'defaultPresentationGrouping', if any.
presentationGroupOf :: AccountTitles -> Maybe PresentationGroup
presentationGroupOf title = fmap pgKey (L.find member defaultPresentationGrouping)
  where
    member def = title `elem` pgGross def || title `elem` pgDeductions def

-- | Look one block up in the grouping in force.
lookupGroupDef :: [PresentationGroupDef] -> PresentationGroup -> Maybe PresentationGroupDef
lookupGroupDef defs key = L.find ((== key) . pgKey) defs

-- | Restrict a grouping to the statement columns a writer renders. A balance
-- sheet must not net revenue accounts, and a profit and loss statement must
-- not net asset accounts.
groupingForDivisions :: [AccountDivision] -> [PresentationGroupDef] -> [PresentationGroupDef]
groupingForDivisions divisions = filter ((`elem` divisions) . pgDivision)

------------------------------------------------------------------
-- Amounts
------------------------------------------------------------------

-- | An amount stated relative to a group's normal side. @raMagnitude@ is
-- always non-negative; @raBelowZero@ says whether the amount sits on the
-- opposite side (a deduction, or a net that its deductions pushed past zero).
-- Callers render the direction — as a leading @-@, a triangle (△), or a move
-- to the other column — and never negate the value itself.
data RelativeAmount v = RelativeAmount
    { raBelowZero :: !Bool
    , raMagnitude :: !v
    }
    deriving (Show, Eq)

-- | Net a @(debit gross, credit gross)@ pair against a side. Uses the same
-- scale-aware near-equality as @ExchangeAlgebra.Write.netGross@\/@diffRL@, so
-- a balance that nets to zero within tolerance reports a zero magnitude
-- rather than floating-point dust.
relativeTo :: HatVal v => Side -> (v, v) -> RelativeAmount v
relativeTo side (debit, credit)
    | nearlyEqScaled debit credit = RelativeAmount False zeroValue
    | otherwise = case side of
        Credit -> orient credit debit
        -- 'defaultSide' only ever answers Debit or Credit, so the wildcard
        -- cannot arise from a group; orient it like Debit rather than
        -- inventing a third direction.
        Debit  -> orient debit credit
        Side   -> orient debit credit
  where
    orient toward away
        | toward >= away = RelativeAmount False (toward - away)
        | otherwise      = RelativeAmount True  (away - toward)

-- | Accumulate @(debit gross, credit gross)@ pairs.
addGross :: Num v => (v, v) -> (v, v) -> (v, v)
addGross (leftDebit, leftCredit) (rightDebit, rightCredit) =
    (leftDebit + rightDebit, leftCredit + rightCredit)

------------------------------------------------------------------
-- Grouped rows
------------------------------------------------------------------

-- | What one row of a block states.
data GroupRowKind
    = GrossRow AccountTitles
      -- ^ A constituent account, before deduction.
    | SubgroupRow PresentationGroup
      -- ^ The net of a nested block, carried into this one.
    | DeductionRow AccountTitles
      -- ^ A contra account. Its amount is already stated relative to the
      -- group's normal side, so it is normally 'raBelowZero'.
    | NetRow PresentationGroup
      -- ^ The block's net. Equals the sum of the rows above it.
    deriving (Show, Eq)

data GroupRow v = GroupRow
    { grKind   :: GroupRowKind
    , grAmount :: RelativeAmount v
    }
    deriving (Show, Eq)

-- | Result of applying a grouping to a set of per-title balances.
data GroupedPresentation v = GroupedPresentation
    { gpBlocks     :: [(PresentationGroupDef, [GroupRow v])]
      -- ^ Blocks in render order: nested blocks precede the block they roll
      -- up into, roots follow the order of the grouping list.
    , gpRootTotals :: Map AccountDivision (v, v)
      -- ^ Per-division @(debit, credit)@ contribution of the __outermost__
      -- blocks only. A caller adds this to the ungrouped part of the column;
      -- adding nested blocks as well would double count.
    , gpConsumed   :: Set AccountTitles
      -- ^ Titles the blocks have taken over. The caller must not render these
      -- again through its ordinary per-entry path.
    }
    deriving (Show, Eq)

-- | Apply a grouping to per-title @(debit gross, credit gross)@ totals.
--
-- A block is /active/ when one of its own contra titles carries non-zero gross
-- activity, or when a nested block is active: a group with nothing to deduct is
-- not a group, and leaving it inactive is what keeps output identical to the
-- pre-amendment behaviour for charts that contain no contra posting. Testing
-- gross activity rather than the net balance also keeps a fully offset contra
-- title inside its group instead of leaking one side into the ordinary rows.
--
-- Complexity: O(g · m + g²) for @g@ blocks of @m@ members (the grouping is a
-- fixed, small table).
presentGroups
    :: HatVal v
    => [PresentationGroupDef]
    -> Map AccountTitles (v, v)
    -> GroupedPresentation v
presentGroups defs balances = GroupedPresentation
    { gpBlocks = blocks
    , gpRootTotals = rootTotals
    , gpConsumed = consumed
    }
  where
    grossOf title = M.findWithDefault (zeroValue, zeroValue) title balances
    hasBalance title = not (uncurry nearlyEqScaled (grossOf title))
    hasActivity title = case M.lookup title balances of
        Nothing -> False
        Just (debit, credit) ->
            not (nearlyEqScaled debit zeroValue)
            || not (nearlyEqScaled credit zeroValue)

    memberTitles def = pgGross def ++ pgDeductions def

    -- Children of a block, in grouping order. 'pgParent' is followed with a
    -- visited set so that a malformed (cyclic) grouping is truncated instead
    -- of diverging.
    childrenOf visited key =
        [ def
        | def <- defs
        , pgParent def == Just key
        , not (S.member (pgKey def) visited)
        ]

    ownPair def = L.foldl' addGross (zeroValue, zeroValue)
        (map grossOf (memberTitles def))

    subtreePair visited def = L.foldl' addGross (ownPair def)
        (map (subtreePair visited') (activeChildrenOf visited' def))
      where
        visited' = S.insert (pgKey def) visited

    ownActive def = any hasActivity (pgDeductions def)

    isActive visited def = ownActive def
        || any (isActive visited') (childrenOf visited' (pgKey def))
      where
        visited' = S.insert (pgKey def) visited

    activeChildrenOf visited def =
        [ child
        | child <- childrenOf visited (pgKey def)
        , pgDivision child == pgDivision def
        , isActive visited child
        ]

    -- A block is a root when it has no usable parent, or when it is the stable
    -- representative of a malformed parent cycle. Cross-division parents are
    -- unusable because a carried subtotal and its root total must share a
    -- statement column.
    isRoot def = case pgParent def of
        Nothing -> True
        Just parent -> case lookupGroupDef defs parent of
            Nothing -> True
            Just parentDef
                | pgDivision parentDef /= pgDivision def -> True
                | otherwise -> case parentCycle def of
                    Nothing -> False
                    Just keys -> pgKey def == minimum keys

    parentCycle def = follow [] (pgKey def)
      where
        follow path key
            | key `elem` path = Just (dropWhile (/= key) path)
            | otherwise = case lookupGroupDef defs key >>= pgParent of
                Nothing -> Nothing
                Just parent -> follow (path ++ [key]) parent

    activeRoots = [def | def <- defs, isRoot def, isActive S.empty def]

    -- Depth-first: nested blocks are rendered before the block that carries
    -- their net, so a reader meets a subtotal only after its constituents.
    blocksOf visited def =
        concatMap (blocksOf visited') activeChildren
        ++ [(def, rowsOf visited' def activeChildren)]
      where
        visited' = S.insert (pgKey def) visited
        activeChildren = activeChildrenOf visited' def

    rowsOf visited def activeChildren =
        [ GroupRow (GrossRow title) (relative (grossOf title))
        | title <- pgGross def, hasBalance title ]
        ++
        [ GroupRow (SubgroupRow (pgKey child)) (relative (subtreePair visited child))
        | child <- activeChildren ]
        ++
        [ GroupRow (DeductionRow title) (relative (grossOf title))
        | title <- pgDeductions def, hasBalance title ]
        ++
        [ GroupRow (NetRow (pgKey def)) (relative (subtreePair visited def)) ]
      where
        relative = relativeTo (groupNormalSide def)

    blocks = concatMap (blocksOf S.empty) activeRoots

    rootTotals = M.fromListWith addGross
        [ (pgDivision def, subtreePair S.empty def) | def <- activeRoots ]

    consumed = S.fromList
        [ title | (def, _) <- blocks, title <- memberTitles def ]
