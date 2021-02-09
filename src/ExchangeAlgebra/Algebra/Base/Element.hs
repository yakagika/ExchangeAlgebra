{- |
    Module     : ExchangeAlgebra.Element
    Copyright  : (c) Kaya Akagi. 2018-2019
    Maintainer : akagi_kaya@icloud.com

    Released under the OWL license

    Package for Exchange Algebra defined by Hirosh Deguch.

    Exchange Algebra is a algebraic description of bokkkeeping system.
    Details are bellow.

    <https://www.springer.com/gp/book/9784431209850>

    <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

    _Note_ : The current version 0.1.0.0 will be completely changed shortly, especially in the accounts settings section.

-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}


module ExchangeAlgebra.Algebra.Base.Element where

import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.Time           as Time
import              Data.Time

------------------------------------------------------------------
-- * Element 基底の要素
------------------------------------------------------------------

-- | Element Class 基底の要素になるためにはこれのインスタンスになる必要がある
class (Eq a, Ord a, Show a) => Element a where

    wiledcard       :: a        -- ^ 検索等に用いるワイルドカード

    isWiledcard     :: a -> Bool
    {-# INLINE isWiledcard #-}
    isWiledcard a | wiledcard == a = True
                  | otherwise      = False

    -- | ワイルドカードからそれ以外への変換
    --  transfer で利用する
    keepWiledcard :: a -> a -> a
    keepWiledcard x y
        | x == y    = x
        | otherwise = case isWiledcard y of
                        True  ->  x
                        False ->  y

    equal :: a -> a -> Bool
    {-# INLINE equal #-}
    equal a b | isWiledcard a = True
              | isWiledcard b = True
              | otherwise     = a == b

    -- | wiledcard を等しいとみなす ==
    (.==) :: a -> a -> Bool
    {-# INLINE (.==)  #-}
    (.==) a b | a == b   || (equal a b) = True
              | otherwise               = False

    -- | wiledcard を等しいとみなす /=
    (./=) :: a -> a -> Bool
    {-# INLINE (./=) #-}
    (./=) a b = not (a .== b)

    compareElement :: a -> a -> Ordering
    {-# INLINE compareElement #-}
    compareElement x y
        | x .== y    = EQ
        | otherwise  = compare x y

    (.<) :: a -> a -> Bool
    (.<) x y | compareElement x y == LT     = True
             | otherwise                    = False

    (.>) :: a -> a -> Bool
    (.>) x y | compareElement x y == GT     = True
             | otherwise                    = False

    (.<=) :: a -> a -> Bool
    (.<=) x y | compareElement x y == LT || compareElement x y == EQ    = True
              | otherwise                                               = False

    (.>=) :: a -> a -> Bool
    (.>=) x y | compareElement x y == GT || compareElement x y == EQ    = True
              | otherwise                                               = False

    maxElement :: a -> a -> a
    maxElement x y  | x .>= y    = x
                    | otherwise  = y

    minElement :: a -> a -> a
    minElement x y  | x .<= y    = x
                    | otherwise  = y

(.#) :: Element a => a
(.#) = wiledcard


infix 4 .==
infix 4 ./=
------------------------------------------------------------------
-- * Elm
------------------------------------------------------------------

-- ** Account Titles

-- | The current version 0.1.0.0 will be completely changed shortly, especially this section.
data  AccountTitles = Cash                            -- ^ 資産 現金
                    | Deposits                        -- ^ 資産 預金
                    | Securities                      -- ^ 資産 有価証券
                    | InvestmentSecurities            -- ^ 資産 投資有価証券
                    | LongTermNationalBonds           -- ^ 資産 長期国債
                    | ShortTermNationalBonds          -- ^ 資産 短期国債
                    | Products                        -- ^ 資産 在庫
                    | Machinery                       -- ^ 資産 機械設備
                    | Building                        -- ^ 資産 不動産
                    | StockInvestment                 -- ^ 資産 株式投資
                    | EquipmentInvestment             -- ^ 資産 設備投資
                    | LongTermLoansReceivable         -- ^ 資産 貸付金
                    | ShortTermLoansReceivable        -- ^ 資産 短期貸付金
                    | ReserveDepositReceivable        -- ^ 資産 預金準備金
                    | Gold                            -- ^ 資産 金
                    | GovernmentService               -- ^ 資産 政府支出
                    | CapitalStock                    -- ^ 資本 資本金
                    | RetainedEarnings                -- ^ 資本 留保所得
                    | LongTermLoansPayable            -- ^ 負債 長期借入金
                    | ShortTermLoansPayable           -- ^ 負債 短期借入金
                    | ReserveForDepreciation          -- ^ 負債 償却準備金
                    | DepositPayable                  -- ^ 負債 預り金
                    | LongTermNationalBondsPayable    -- ^ 負債 長期国債 借入金
                    | ShortTermNationalBondsPayable   -- ^ 負債 短期国債 借入金
                    | ReserveDepositPayable           -- ^ 負債 未払金
                    | CentralBankNotePayable          -- ^ 負債 中央銀行手形
                    | Depreciation                    -- ^ 費用
                    | WageExpenditure                 -- ^ 費用
                    | InterestExpense                 -- ^ 費用
                    | TaxesExpense                    -- ^ 費用
                    | ConsumptionExpenditure          -- ^ 費用
                    | SubsidyExpense                  -- ^ 費用
                    | CentralBankPaymentExpense       -- ^ 費用
                    | ValueAdded                      -- ^ 収益
                    | SubsidyIncome                   -- ^ 収益
                    | NationalBondInterestEarned      -- ^ 収益
                    | DepositInterestEarned           -- ^ 収益
                    | GrossProfit                     -- ^ 収益
                    | OrdinaryProfit                  -- ^ 収益
                    | InterestEarned                  -- ^ 収益
                    | WageEarned                      -- ^ 収益
                    | TaxesRevenue                    -- ^ 収益
                    | CentralBankPaymentIncome        -- ^ 収益
                    | AccountTitle                    -- ^ ワイルドカード
                    deriving (Show, Ord, Eq, Enum)

instance Element AccountTitles where
    wiledcard = AccountTitle

{- |

勘定科目の全体と
必要となる処理に関して,体系的に包括された参考文献
それをコピーすれば必要十分な文献

-}

-- | Name :: 品目の名前
type Name = Text

-- | 勘定科目の主体
type Subject = Text
instance Element Text where
    wiledcard   = T.empty

-- | 通貨単位 又は 物量
data CountUnit  = Yen
                | Dollar
                | Euro
                | CNY
                | Amount
                | CountUnit
                deriving (Show, Ord, Eq, Enum)

instance Element CountUnit where
    wiledcard = CountUnit

instance Element TimeOfDay where
    wiledcard = Time.midnight

instance Element Day where
    wiledcard =  ModifiedJulianDay 0

instance (Element a ,Element b)
    => Element (a, b) where
    wiledcard = (wiledcard, wiledcard)

    {-# INLINE equal #-}
    equal (a1, a2) (b1, b2)
        =  (a1 .== b1)
        && (a2 .== b2)

    keepWiledcard (a1, a2) (b1, b2)
        = ( keepWiledcard a1 b1
          , keepWiledcard a2 b2)

    compareElement (a1, a2) (b1, b2)
        = case compareElement a1 b1 of
            EQ -> compareElement a2 b2
            x  -> x

instance (Element a, Element b, Element c)
    => Element (a, b, c) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE equal #-}
    equal (a1, a2, a3) (b1, b2, b3)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)

    keepWiledcard (a1, a2, a3) (b1, b2, b3)
        = ( keepWiledcard a1 b1
          , keepWiledcard a2 b2
          , keepWiledcard a3 b3)

    compareElement (a1, a2, a3) (b1, b2, b3)
        = compareElement ((a1, a2), a3)
                         ((b1, b2), b3)


instance (Element a, Element b, Element c, Element d)
    => Element (a, b, c, d) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4) (b1, b2, b3, b4)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)

    keepWiledcard (a1, a2, a3, a4) (b1, b2, b3, b4)
        = ( keepWiledcard a1 b1
          , keepWiledcard a2 b2
          , keepWiledcard a3 b3
          , keepWiledcard a4 b4)

    compareElement (a1, a2, a3, a4) (b1, b2, b3, b4)
        = compareElement ((a1, a2, a3), a4)
                         ((b1, b2, b3), b4)


instance (Element a, Element b, Element c, Element d, Element e)
    => Element (a, b, c, d, e) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)

    keepWiledcard (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = ( keepWiledcard a1 b1
          , keepWiledcard a2 b2
          , keepWiledcard a3 b3
          , keepWiledcard a4 b4
          , keepWiledcard a5 b5)

    compareElement (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = compareElement ((a1, a2, a3, a4), a5)
                         ((b1, b2, b3, b4), b5)


instance (Element a, Element b, Element c, Element d, Element e, Element f)
    => Element (a, b, c, d, e, f) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)

    keepWiledcard (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = ( keepWiledcard a1 b1
          , keepWiledcard a2 b2
          , keepWiledcard a3 b3
          , keepWiledcard a4 b4
          , keepWiledcard a5 b5
          , keepWiledcard a6 b6)

    compareElement (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = compareElement ((a1, a2, a3, a4, a5), a6)
                         ((b1, b2, b3, b4, b5), b6)


instance (Element a, Element b, Element c, Element d, Element e, Element f, Element d)
    => Element (a, b, c, d, e, f, d) where
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)
    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)
        && (a7 .== b7)

    keepWiledcard (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = ( keepWiledcard a1 b1
          , keepWiledcard a2 b2
          , keepWiledcard a3 b3
          , keepWiledcard a4 b4
          , keepWiledcard a5 b5
          , keepWiledcard a6 b6
          , keepWiledcard a7 b7)

    compareElement (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = compareElement ((a1, a2, a3, a4, a5, a6), a7)
                         ((b1, b2, b3, b4, b5, b6), b7)
