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
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE DeriveGeneric              #-}

module ExchangeAlgebra.Algebra.Base.Element
    ( module ExchangeAlgebra.Algebra.Base.Element
    , module Data.Hashable
    , module GHC.Generics) where

import qualified    Data.Text           as T
import              Data.Text           (Text)
import qualified    Data.Time           as Time
import              Data.Time
import GHC.Generics (Generic)
import Data.Hashable
import Data.Typeable (Typeable, cast, typeOf)
import qualified Data.Binary as Binary

------------------------------------------------------------------
-- * Element 基底の要素
------------------------------------------------------------------

-- | Element Class 基底の要素になるためにはこれのインスタンスになる必要がある
class (Eq a, Ord a, Show a, Hashable a, Typeable a) => Element a where

    wiledcard       :: a        -- ^ 検索等に用いるワイルドカード

    {-# INLINE haveWiledcard #-}
    haveWiledcard :: a -> Bool
    haveWiledcard = isWiledcard

    {-# INLINE isWiledcard #-}
    isWiledcard     :: a -> Bool
    isWiledcard a = a == wiledcard

    -- | ワイルドカードを無視した変換
    --  transfer で利用する
    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard :: a -> a -> a
    ignoreWiledcard before after
        | before == after   = before
        | isWiledcard after = before
        | otherwise         = after

    {-# INLINE equal #-}
    equal :: a -> a -> Bool
    equal a b | isWiledcard a = True
              | isWiledcard b = True
              | otherwise     = a == b

    -- | wiledcard を等しいとみなす ==
    {-# INLINE (.==)  #-}
    (.==) :: a -> a -> Bool
    (.==) a b = a == b || (haveWiledcard a || haveWiledcard b) && equal a b

    -- | wiledcard を等しいとみなす /=
    {-# INLINE (./=) #-}
    (./=) :: a -> a -> Bool
    (./=) a b = not (a .== b)

    {-# INLINE compareElement #-}
    compareElement :: a -> a -> Ordering
    compareElement x y
        | x .== y = EQ
        | otherwise = compare x y

    (.<) :: a -> a -> Bool
    (.<) x y = compareElement x y == LT

    (.>) :: a -> a -> Bool
    (.>) x y = compareElement x y == GT

    (.<=) :: a -> a -> Bool
    (.<=) x y = compareElement x y /= GT

    (.>=) :: a -> a -> Bool
    (.>=) x y = compareElement x y /= LT

    maxElement :: a -> a -> a
    maxElement x y
        | x .>= y = x
        | otherwise = y

    minElement :: a -> a -> a
    minElement x y
        | x .<= y = x
        | otherwise = y

data AxisKey = forall a. Element a => AxisKey !a

instance Eq AxisKey where
    AxisKey x == AxisKey y = case cast y of
        Nothing -> False
        Just y' -> x == y'

instance Hashable AxisKey where
    hashWithSalt salt (AxisKey x) = salt `hashWithSalt` (typeOf x) `hashWithSalt` x

{-# INLINE axisIsWildcard #-}
axisIsWildcard :: AxisKey -> Bool
axisIsWildcard (AxisKey x) = isWiledcard x

class (Element a) => AxisDecompose a where
    toAxisKeys :: a -> [AxisKey]

instance {-# OVERLAPPABLE #-} Element a => AxisDecompose a where
    {-# INLINE toAxisKeys #-}
    toAxisKeys a = [AxisKey a]

{-# INLINE (.#) #-}
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
                    | Deposits                        -- ^ 資産 普通預金
                    | CurrentDeposits                 -- ^ 資産 当座預金
                    | Securities                      -- ^ 資産 有価証券
                    | InvestmentSecurities            -- ^ 資産 投資有価証券
                    | LongTermNationalBonds           -- ^ 資産 長期国債
                    | ShortTermNationalBonds          -- ^ 資産 短期国債
                    | Products                        -- ^ 資産 商品
                    | Machinery                       -- ^ 資産 機械設備 備品
                    | Building                        -- ^ 資産 不動産
                    | Vehicle                         -- ^ 資産 車両 運搬具
                    | StockInvestment                 -- ^ 資産 株式投資
                    | EquipmentInvestment             -- ^ 資産 設備投資
                    | LongTermLoansReceivable         -- ^ 資産 貸付金
                    | AccountsReceivable              -- ^ 資産 債権 売掛金
                    | ShortTermLoansReceivable        -- ^ 資産 短期貸付金
                    | ReserveDepositReceivable        -- ^ 資産 預金準備金
                    | Gold                            -- ^ 資産 金
                    | GovernmentService               -- ^ 資産 政府支出
                    | CapitalStock                    -- ^ 資本 資本金
                    | RetainedEarnings                -- ^ 資本 利益剰余金
                    | LongTermLoansPayable            -- ^ 負債 長期借入金
                    | ShortTermLoansPayable           -- ^ 負債 短期借入金
                    | LoansPayable                    -- ^ 負債 借入金
                    | ReserveForDepreciation          -- ^ 負債 償却準備金
                    | DepositPayable                  -- ^ 負債 預り金
                    | LongTermNationalBondsPayable    -- ^ 負債 長期国債 借入金
                    | ShortTermNationalBondsPayable   -- ^ 負債 短期国債 借入金
                    | ReserveDepositPayable           -- ^ 負債 未払金
                    | CentralBankNotePayable          -- ^ 負債 中央銀行手形
                    | Depreciation                    -- ^ 費用 減価償却費
                    | SalesCost                       -- ^ 費用 売上原価
                    | BusinessTrip                    -- ^ 費用 旅費交通費
                    | Commutation                     -- ^ 費用 通信費
                    | UtilitiesExpense                -- ^ 費用 水道光熱費
                    | RentExpense                     -- ^ 費用 支払家賃
                    | AdvertisingExpense              -- ^ 費用 広告宣伝費
                    | DeliveryExpenses                -- ^ 費用 発送費
                    | SuppliesExpenses                -- ^ 費用 消耗品費
                    | MiscellaneousExpenses           -- ^ 費用 雑費
                    | WageExpenditure                 -- ^ 費用 給料
                    | InterestExpense                 -- ^ 費用 支払利息
                    | TaxesExpense                    -- ^ 費用 税
                    | ConsumptionExpenditure          -- ^ 費用 消耗品費
                    | SubsidyExpense                  -- ^ 費用 補助金支出
                    | CentralBankPaymentExpense       -- ^ 費用
                    | Purchases                       -- ^ 費用 仕入
                    | NetIncome                       -- ^ 費用 当期純利益
                    | ValueAdded                      -- ^ 収益 付加価値
                    | SubsidyIncome                   -- ^ 収益 補助金
                    | NationalBondInterestEarned      -- ^ 収益 国際受取利息
                    | DepositInterestEarned           -- ^ 収益 預金受取利息
                    | GrossProfit                     -- ^ 収益 売上総利益
                    | OrdinaryProfit                  -- ^ 収益 経常利益
                    | InterestEarned                  -- ^ 収益 受取利息
                    | ReceiptFee                      -- ^ 収益 受取手数料
                    | RentalIncome                    -- ^ 収益 受取家賃
                    | WageEarned                      -- ^ 収益 賃金収入
                    | TaxesRevenue                    -- ^ 収益 税収
                    | CentralBankPaymentIncome        -- ^ 収益
                    | Sales                           -- ^ 収益 売上
                    | NetLoss                         -- ^ 収益 当期純損失
                    | AccountTitle                    -- ^ ワイルドカード
                    deriving (Show, Ord, Eq, Enum, Generic)

instance Hashable AccountTitles where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt x = hashWithSalt salt (fromEnum x)

instance Binary.Binary AccountTitles where
    {-# INLINE put #-}
    put = Binary.putWord8 . fromIntegral . fromEnum
    {-# INLINE get #-}
    get = toEnum . fromIntegral <$> Binary.getWord8

instance Element AccountTitles where
    {-# INLINE wiledcard #-}
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

    {-# INLINE wiledcard #-}
    wiledcard   = T.empty

-- | 通貨単位 又は 物量
data CountUnit  = Yen
                | Dollar
                | Euro
                | CNY
                | Amount
                | CountUnit
                deriving (Show, Ord, Eq, Enum,Generic)

instance Hashable CountUnit where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt x = hashWithSalt salt (fromEnum x)

instance Binary.Binary CountUnit where
    {-# INLINE put #-}
    put = Binary.putWord8 . fromIntegral . fromEnum
    {-# INLINE get #-}
    get = toEnum . fromIntegral <$> Binary.getWord8

instance Element CountUnit where

    {-# INLINE wiledcard #-}
    wiledcard = CountUnit


-- TimeOfDay は内部で時,分,秒 (Pico) を保持しているので,それぞれをハッシュ化する
instance Hashable TimeOfDay where
  hashWithSalt salt (TimeOfDay hour min sec) =
    salt `hashWithSalt` hour `hashWithSalt` min `hashWithSalt` sec

-- Day は内部で ModifiedJulianDay 形式の Integer を保持しているのでそれを利用
instance Hashable Day where
  hashWithSalt salt day = hashWithSalt salt (toModifiedJulianDay day)

instance Element TimeOfDay where
    wiledcard = Time.midnight

instance Element Day where
    wiledcard =  ModifiedJulianDay 0

instance (Element a ,Element b)
    => Element (a, b) where

    {-# INLINE wiledcard #-}
    wiledcard = (wiledcard, wiledcard)

    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b)
        = isWiledcard a
       || isWiledcard b

    {-# INLINE equal #-}
    equal (a1, a2) (b1, b2)
        =  (a1 .== b1)
        && (a2 .== b2)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2) (b1, b2)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2)

    {-# INLINE compareElement #-}
    compareElement (a1, a2) (b1, b2)
        = case compareElement a1 b1 of
            EQ -> compareElement a2 b2
            x  -> x

instance {-# OVERLAPPING #-} (Element a, Element b)
    => AxisDecompose (a, b) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b) = [AxisKey a, AxisKey b]

instance (Element a, Element b, Element c)
    => Element (a, b, c) where

    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c


    {-# INLINE equal #-}
    equal (a1, a2, a3) (b1, b2, b3)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3) (b1, b2, b3)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3) (b1, b2, b3)
        = compareElement ((a1, a2), a3)
                         ((b1, b2), b3)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c)
    => AxisDecompose (a, b, c) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c) = [AxisKey a, AxisKey b, AxisKey c]


instance (Element a, Element b, Element c, Element d)
    => Element (a, b, c, d) where

    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)


    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c,d)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c
       || isWiledcard d

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4) (b1, b2, b3, b4)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3, a4) (b1, b2, b3, b4)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3
          , ignoreWiledcard a4 b4)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4) (b1, b2, b3, b4)
        = compareElement ((a1, a2, a3), a4)
                         ((b1, b2, b3), b4)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d)
    => AxisDecompose (a, b, c, d) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d]


instance (Element a, Element b, Element c, Element d, Element e)
    => Element (a, b, c, d, e) where

    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)


    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c,d,e)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c
       || isWiledcard d
       || isWiledcard e

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3
          , ignoreWiledcard a4 b4
          , ignoreWiledcard a5 b5)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5)
        = compareElement ((a1, a2, a3, a4), a5)
                         ((b1, b2, b3, b4), b5)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e)
    => AxisDecompose (a, b, c, d, e) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e]


instance (Element a, Element b, Element c, Element d, Element e, Element f)
    => Element (a, b, c, d, e, f) where

    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c,d,e,f)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c
       || isWiledcard d
       || isWiledcard e
       || isWiledcard f

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3
          , ignoreWiledcard a4 b4
          , ignoreWiledcard a5 b5
          , ignoreWiledcard a6 b6)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6)
        = compareElement ((a1, a2, a3, a4, a5), a6)
                         ((b1, b2, b3, b4, b5), b6)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e, Element f)
    => AxisDecompose (a, b, c, d, e, f) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e, f) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e, AxisKey f]


instance (Element a, Element b, Element c, Element d, Element e, Element f, Element g)
    => Element (a, b, c, d, e, f, g) where
    {-# INLINE wiledcard #-}
    wiledcard = ( wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard
                , wiledcard)

    {-# INLINE haveWiledcard #-}
    haveWiledcard (a,b,c,d,e,f,g)
        = isWiledcard a
       || isWiledcard b
       || isWiledcard c
       || isWiledcard d
       || isWiledcard e
       || isWiledcard f
       || isWiledcard g

    {-# INLINE equal #-}
    equal (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        =  (a1 .== b1)
        && (a2 .== b2)
        && (a3 .== b3)
        && (a4 .== b4)
        && (a5 .== b5)
        && (a6 .== b6)
        && (a7 .== b7)

    {-# INLINE ignoreWiledcard #-}
    ignoreWiledcard (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = ( ignoreWiledcard a1 b1
          , ignoreWiledcard a2 b2
          , ignoreWiledcard a3 b3
          , ignoreWiledcard a4 b4
          , ignoreWiledcard a5 b5
          , ignoreWiledcard a6 b6
          , ignoreWiledcard a7 b7)

    {-# INLINE compareElement #-}
    compareElement (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7)
        = compareElement ((a1, a2, a3, a4, a5, a6), a7)
                         ((b1, b2, b3, b4, b5, b6), b7)

instance {-# OVERLAPPING #-} (Element a, Element b, Element c, Element d, Element e, Element f, Element g)
    => AxisDecompose (a, b, c, d, e, f, g) where
    {-# INLINE toAxisKeys #-}
    toAxisKeys (a, b, c, d, e, f, g) = [AxisKey a, AxisKey b, AxisKey c, AxisKey d, AxisKey e, AxisKey f, AxisKey g]
