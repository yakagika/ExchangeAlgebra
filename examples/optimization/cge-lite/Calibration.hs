{- |
  Calibration — the Hosoe Ch.6 SAM and its calibration block, ported from
  @optimization/CGE/GAMS/stdcge.gms@ (GE plan @phase1-cge-reproduction@,
  task 1a).

  == Design of record

  Every definition here mirrors one GAMS statement of @stdcge.gms@, in the
  same order and under the same name (the math-correspondence convention:
  the Haskell text should be checkable against the GAMS text line by line).
  The GAMS structure is kept deliberately:

  * one universe set @u@ (SAM accounts) with /subset/ sets @i(u)@ (goods)
    and @h(u)@ (factors) — so 'Account' is the one enumeration and 'goods'\/
    'factors' are subset lists, exactly like @Set u \/ i(u) \/ h(u)@; per-good
    values are @'M.Map' 'Account' Double@ built over those lists, not fresh
    index types.
  * the \"Loading the initial values\" block -> 'Levels0'.
  * the \"Calibration\" block (elasticities + share\/scale parameters) ->
    'Params'.

  Ground truth for all of this:

  * @GAMS\/stdcge.lst@ — the @display@ output of every calibrated parameter
    (3-decimal rounding), cross-checked by the @cge-lite-test@ suite.
  * @GAMS\/results.csv@ — the solved benchmark (all prices 1.0,
    @UU = 25.5085...@); its quantity columns must equal 'Levels0' exactly
    (the SAM /is/ the benchmark equilibrium), also enforced by the test.

  == Scope note (1a, not 1b)

  This module is data and arithmetic only — no 'Journal', no stages, no
  solver. @CGELite.hs@ (task 1b) consumes 'calibration' through its
  @CGEParams@; the sentinel test consumes it directly.
-}
module Calibration
    ( -- * Sets (GAMS @Set u, i(u), h(u)@)
      Account (..)
    , accounts, goods, factors
      -- * The social accounting matrix (GAMS @Table SAM(u,v)@)
    , sam, samRowSum, samColSum
      -- * Initial values (GAMS \"Loading the initial values\")
    , Levels0 (..)
      -- * Calibrated parameters (GAMS \"Calibration\")
    , Params (..)
      -- * The calibration result
    , Calibration (..)
    , calibration
      -- * Benchmark utility (GAMS @UU@ at the benchmark point)
    , benchmarkUtility
    ) where

import qualified Data.Map.Strict as M

------------------------------------------------------------------
-- * Sets
------------------------------------------------------------------

-- | The ten SAM accounts (GAMS @Set u@): two goods, two factors, indirect
-- tax (@IDT@), import tariff (@TRF@), and the four institutions.
data Account
    = BRD  -- ^ Bread (good).
    | MLK  -- ^ Milk (good).
    | CAP  -- ^ Capital (factor).
    | LAB  -- ^ Labor (factor).
    | IDT  -- ^ Indirect (production) tax account.
    | TRF  -- ^ Import tariff account.
    | HOH  -- ^ Household.
    | GOV  -- ^ Government.
    | INV  -- ^ Investment.
    | EXT  -- ^ Rest of world.
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | GAMS @u@.
accounts :: [Account]
accounts = [minBound .. maxBound]

-- | GAMS @i(u)@ — the goods subset.
goods :: [Account]
goods = [BRD, MLK]

-- | GAMS @h(u)@ — the factor subset.
factors :: [Account]
factors = [CAP, LAB]

------------------------------------------------------------------
-- * SAM
------------------------------------------------------------------

-- | GAMS @Table SAM(u,v)@, row by row; absent cells are 0 (GAMS blank).
-- Rows are sellers\/receivers, columns are buyers\/payers.
samTable :: M.Map (Account, Account) Double
samTable = M.fromList
    [ ((BRD, BRD), 21), ((BRD, MLK),  8), ((BRD, HOH), 20), ((BRD, GOV), 19), ((BRD, INV), 16), ((BRD, EXT),  8)
    , ((MLK, BRD), 17), ((MLK, MLK),  9), ((MLK, HOH), 30), ((MLK, GOV), 14), ((MLK, INV), 15), ((MLK, EXT),  4)
    , ((CAP, BRD), 20), ((CAP, MLK), 30)
    , ((LAB, BRD), 15), ((LAB, MLK), 25)
    , ((IDT, BRD),  5), ((IDT, MLK),  4)
    , ((TRF, BRD),  1), ((TRF, MLK),  2)
    , ((HOH, CAP), 50), ((HOH, LAB), 40)
    , ((GOV, IDT),  9), ((GOV, TRF),  3), ((GOV, HOH), 23)
    , ((INV, HOH), 17), ((INV, GOV),  2), ((INV, EXT), 12)
    , ((EXT, BRD), 13), ((EXT, MLK), 11)
    ]

-- | @sam u v@ — the SAM cell, 0 when blank.
sam :: Account -> Account -> Double
sam u v = M.findWithDefault 0 (u, v) samTable

-- | Row sum (total receipts of account @u@). A consistent SAM has
-- @'samRowSum' u == 'samColSum' u@ for every @u@ (checked by the test).
samRowSum :: Account -> Double
samRowSum u = sum [sam u v | v <- accounts]

-- | Column sum (total payments of account @u@).
samColSum :: Account -> Double
samColSum u = sum [sam v u | v <- accounts]

------------------------------------------------------------------
-- * Initial values (GAMS "Loading the initial values")
------------------------------------------------------------------

-- | The benchmark levels read off the SAM — one field per GAMS parameter of
-- the \"Loading the initial values\" block, same names (@Y0@ -> 'y0', ...).
-- Goods-indexed fields are total over 'goods', factor-indexed over
-- 'factors'; two-indexed fields are @(h, j)@ resp. @(i, j)@ keyed.
data Levels0 = Levels0
    { y0   :: !(M.Map Account Double)             -- ^ @Y0(j)@ composite factor.
    , f0   :: !(M.Map (Account, Account) Double)  -- ^ @F0(h,j)@ factor input.
    , x0   :: !(M.Map (Account, Account) Double)  -- ^ @X0(i,j)@ intermediate input.
    , z0   :: !(M.Map Account Double)             -- ^ @Z0(j)@ gross output.
    , xp0  :: !(M.Map Account Double)             -- ^ @Xp0(i)@ household consumption.
    , xg0  :: !(M.Map Account Double)             -- ^ @Xg0(i)@ government consumption.
    , xv0  :: !(M.Map Account Double)             -- ^ @Xv0(i)@ investment demand.
    , e0   :: !(M.Map Account Double)             -- ^ @E0(i)@ exports.
    , m0   :: !(M.Map Account Double)             -- ^ @M0(i)@ imports.
    , q0   :: !(M.Map Account Double)             -- ^ @Q0(i)@ Armington composite.
    , d0   :: !(M.Map Account Double)             -- ^ @D0(i)@ domestic good.
    , sp0  :: !Double                             -- ^ @Sp0@ private saving.
    , sg0  :: !Double                             -- ^ @Sg0@ government saving.
    , td0  :: !Double                             -- ^ @Td0@ direct tax.
    , tz0  :: !(M.Map Account Double)             -- ^ @Tz0(j)@ production tax.
    , tm0  :: !(M.Map Account Double)             -- ^ @Tm0(j)@ import tariff.
    , ff   :: !(M.Map Account Double)             -- ^ @FF(h)@ factor endowment.
    , sf   :: !Double                             -- ^ @Sf@ foreign saving (US$).
    , pWe  :: !(M.Map Account Double)             -- ^ @pWe(i)@ world export price (US$).
    , pWm  :: !(M.Map Account Double)             -- ^ @pWm(i)@ world import price (US$).
    , tauz :: !(M.Map Account Double)             -- ^ @tauz(i)@ production tax rate.
    , taum :: !(M.Map Account Double)             -- ^ @taum(i)@ import tariff rate.
    } deriving (Eq, Show)

-- | The GAMS assignment block, statement by statement. Local names shadow
-- nothing; each @let@ is one GAMS line, in GAMS order (dependencies flow
-- top to bottom exactly as in the source).
levels0 :: Levels0
levels0 = Levels0
    { y0 = y0', f0 = f0', x0 = x0', z0 = z0'
    , xp0 = xp0', xg0 = xg0', xv0 = xv0'
    , e0 = e0', m0 = m0', q0 = q0', d0 = d0'
    , sp0 = sp0', sg0 = sg0', td0 = td0', tz0 = tz0', tm0 = tm0'
    , ff = ff', sf = sf', pWe = pWe', pWm = pWm'
    , tauz = tauz', taum = taum'
    }
  where
    overGoods, overFactors :: (Account -> Double) -> M.Map Account Double
    overGoods   g = M.fromList [(i, g i) | i <- goods]
    overFactors g = M.fromList [(h, g h) | h <- factors]

    -- Td0     = SAM("GOV","HOH");
    td0' = sam GOV HOH
    -- Tz0(j)  = SAM("IDT",j);
    tz0' = overGoods $ \j -> sam IDT j
    -- Tm0(j)  = SAM("TRF",J);
    tm0' = overGoods $ \j -> sam TRF j
    -- F0(h,j) = SAM(h,j);
    f0'  = M.fromList [((h, j), sam h j) | h <- factors, j <- goods]
    -- Y0(j)   = sum(h, F0(h,j));
    y0'  = overGoods $ \j -> sum [f0' M.! (h, j) | h <- factors]
    -- X0(i,j) = SAM(i,j);
    x0'  = M.fromList [((i, j), sam i j) | i <- goods, j <- goods]
    -- Z0(j)   = Y0(j) +sum(i, X0(i,j));
    z0'  = overGoods $ \j -> y0' M.! j + sum [x0' M.! (i, j) | i <- goods]
    -- M0(i)   = SAM("EXT",i);
    m0'  = overGoods $ \i -> sam EXT i
    -- tauz(j) = Tz0(j)/Z0(j);
    tauz' = overGoods $ \j -> tz0' M.! j / z0' M.! j
    -- taum(j) = Tm0(j)/M0(j);
    taum' = overGoods $ \j -> tm0' M.! j / m0' M.! j
    -- Xp0(i)  = SAM(i,"HOH");
    xp0' = overGoods $ \i -> sam i HOH
    -- FF(h)   = SAM("HOH",h);
    ff'  = overFactors $ \h -> sam HOH h
    -- Xg0(i)  = SAM(i,"GOV");
    xg0' = overGoods $ \i -> sam i GOV
    -- Xv0(i)  = SAM(i,"INV");
    xv0' = overGoods $ \i -> sam i INV
    -- E0(i)   = SAM(i,"EXT");
    e0'  = overGoods $ \i -> sam i EXT
    -- Q0(i)   = Xp0(i)+Xg0(i)+Xv0(i)+sum(j, X0(i,j));
    q0'  = overGoods $ \i ->
             xp0' M.! i + xg0' M.! i + xv0' M.! i + sum [x0' M.! (i, j) | j <- goods]
    -- D0(i)   = (1+tauz(i))*Z0(i)-E0(i);
    d0'  = overGoods $ \i -> (1 + tauz' M.! i) * z0' M.! i - e0' M.! i
    -- Sp0     = SAM("INV","HOH");
    sp0' = sam INV HOH
    -- Sg0     = SAM("INV","GOV");
    sg0' = sam INV GOV
    -- Sf      = SAM("INV","EXT");
    sf'  = sam INV EXT
    -- pWe(i)  = 1;  pWm(i)  = 1;
    pWe' = overGoods $ const 1
    pWm' = overGoods $ const 1

------------------------------------------------------------------
-- * Calibrated parameters (GAMS "Calibration")
------------------------------------------------------------------

-- | The calibrated parameters — one field per GAMS parameter of the
-- \"Calibration\" block, same names. The four elasticity fields come first
-- (the GAMS block sets @sigma = psi = 2@ exogenously and derives
-- @eta@\/@phi@); everything after is derived from 'Levels0'.
data Params = Params
    { sigma  :: !(M.Map Account Double)            -- ^ @sigma(i)@ Armington substitution elasticity (= 2).
    , psi    :: !(M.Map Account Double)            -- ^ @psi(i)@ CET transformation elasticity (= 2).
    , eta    :: !(M.Map Account Double)            -- ^ @eta(i) = (sigma-1)\/sigma@.
    , phi    :: !(M.Map Account Double)            -- ^ @phi(i) = (psi+1)\/psi@.
    , alpha  :: !(M.Map Account Double)            -- ^ @alpha(i)@ utility share.
    , beta   :: !(M.Map (Account, Account) Double) -- ^ @beta(h,j)@ production share.
    , b      :: !(M.Map Account Double)            -- ^ @b(j)@ production scale.
    , ax     :: !(M.Map (Account, Account) Double) -- ^ @ax(i,j)@ intermediate input coeff.
    , ay     :: !(M.Map Account Double)            -- ^ @ay(j)@ composite-factor input coeff.
    , mu     :: !(M.Map Account Double)            -- ^ @mu(i)@ government consumption share.
    , lambda :: !(M.Map Account Double)            -- ^ @lambda(i)@ investment demand share.
    , deltam :: !(M.Map Account Double)            -- ^ @deltam(i)@ Armington share (imports).
    , deltad :: !(M.Map Account Double)            -- ^ @deltad(i)@ Armington share (domestic).
    , gamma  :: !(M.Map Account Double)            -- ^ @gamma(i)@ Armington scale.
    , xie    :: !(M.Map Account Double)            -- ^ @xie(i)@ CET share (exports).
    , xid    :: !(M.Map Account Double)            -- ^ @xid(i)@ CET share (domestic).
    , theta  :: !(M.Map Account Double)            -- ^ @theta(i)@ CET scale.
    , ssp    :: !Double                            -- ^ @ssp@ private saving propensity.
    , ssg    :: !Double                            -- ^ @ssg@ government saving propensity.
    , taud   :: !Double                            -- ^ @taud@ direct tax rate.
    } deriving (Eq, Show)

-- | The GAMS calibration block, statement by statement (same order; each
-- local binding is one GAMS line).
params :: Levels0 -> Params
params l = Params
    { sigma = sigma', psi = psi', eta = eta', phi = phi'
    , alpha = alpha', beta = beta', b = b', ax = ax', ay = ay'
    , mu = mu', lambda = lambda'
    , deltam = deltam', deltad = deltad', gamma = gamma'
    , xie = xie', xid = xid', theta = theta'
    , ssp = ssp', ssg = ssg', taud = taud'
    }
  where
    overGoods :: (Account -> Double) -> M.Map Account Double
    overGoods g = M.fromList [(i, g i) | i <- goods]

    -- sigma(i) =  2;  psi(i)   =  2;
    sigma' = overGoods $ const 2
    psi'   = overGoods $ const 2
    -- eta(i)   = (sigma(i) - 1)/sigma(i);
    eta'   = overGoods $ \i -> (sigma' M.! i - 1) / sigma' M.! i
    -- phi(i)   = (psi(i) + 1)/psi(i);
    phi'   = overGoods $ \i -> (psi' M.! i + 1) / psi' M.! i

    -- alpha(i)  =  Xp0(i)/sum(j, Xp0(j));
    alpha' = overGoods $ \i -> xp0 l M.! i / sum [xp0 l M.! j | j <- goods]
    -- beta(h,j) =  F0(h,j)/sum(k, F0(k,j));
    beta'  = M.fromList
        [ ((h, j), f0 l M.! (h, j) / sum [f0 l M.! (k, j) | k <- factors])
        | h <- factors, j <- goods ]
    -- b(j)      =  Y0(j)/prod(h, F0(h,j)**beta(h,j));
    b'     = overGoods $ \j ->
               y0 l M.! j / product [f0 l M.! (h, j) ** (beta' M.! (h, j)) | h <- factors]
    -- ax(i,j)   =  X0(i,j)/Z0(j);
    ax'    = M.fromList [((i, j), x0 l M.! (i, j) / z0 l M.! j) | i <- goods, j <- goods]
    -- ay(j)     =  Y0(j)/Z0(j);
    ay'    = overGoods $ \j -> y0 l M.! j / z0 l M.! j
    -- mu(i)     =  Xg0(i)/sum(j, Xg0(j));
    mu'    = overGoods $ \i -> xg0 l M.! i / sum [xg0 l M.! j | j <- goods]
    -- lambda(i) =  Xv0(i)/(Sp0+Sg0+Sf);
    lambda' = overGoods $ \i -> xv0 l M.! i / (sp0 l + sg0 l + sf l)
    -- deltam(i) = (1+taum(i))*M0(i)**(1-eta(i))/((1+taum(i))*M0(i)**(1-eta(i)) + D0(i)**(1-eta(i)));
    deltam' = overGoods $ \i ->
        let num = (1 + taum l M.! i) * (m0 l M.! i ** (1 - eta' M.! i))
        in  num / (num + d0 l M.! i ** (1 - eta' M.! i))
    -- deltad(i) =  D0(i)**(1-eta(i))/((1+taum(i))*M0(i)**(1-eta(i)) + D0(i)**(1-eta(i)));
    deltad' = overGoods $ \i ->
        let numM = (1 + taum l M.! i) * (m0 l M.! i ** (1 - eta' M.! i))
        in  (d0 l M.! i ** (1 - eta' M.! i)) / (numM + d0 l M.! i ** (1 - eta' M.! i))
    -- gamma(i)  =  Q0(i)/(deltam(i)*M0(i)**eta(i)+deltad(i)*D0(i)**eta(i))**(1/eta(i));
    gamma'  = overGoods $ \i ->
        q0 l M.! i
        / (  deltam' M.! i * (m0 l M.! i ** (eta' M.! i))
           + deltad' M.! i * (d0 l M.! i ** (eta' M.! i))
          ) ** (1 / eta' M.! i)
    -- xie(i)    =  E0(i)**(1-phi(i))/(E0(i)**(1-phi(i))+D0(i)**(1-phi(i)));
    xie'    = overGoods $ \i ->
        (e0 l M.! i ** (1 - phi' M.! i))
        / (e0 l M.! i ** (1 - phi' M.! i) + d0 l M.! i ** (1 - phi' M.! i))
    -- xid(i)    =  D0(i)**(1-phi(i))/(E0(i)**(1-phi(i))+D0(i)**(1-phi(i)));
    xid'    = overGoods $ \i ->
        (d0 l M.! i ** (1 - phi' M.! i))
        / (e0 l M.! i ** (1 - phi' M.! i) + d0 l M.! i ** (1 - phi' M.! i))
    -- theta(i)  =  Z0(i)/(xie(i)*E0(i)**phi(i)+xid(i)*D0(i)**phi(i))**(1/phi(i));
    theta'  = overGoods $ \i ->
        z0 l M.! i
        / (  xie' M.! i * (e0 l M.! i ** (phi' M.! i))
           + xid' M.! i * (d0 l M.! i ** (phi' M.! i))
          ) ** (1 / phi' M.! i)
    -- ssp       =  Sp0/sum(h, FF(h));
    ssp'    = sp0 l / sum [ff l M.! h | h <- factors]
    -- ssg       =  Sg0/(Td0+sum(j, Tz0(j))+sum(j, Tm0(j)));
    ssg'    = sg0 l / (td0 l + sum [tz0 l M.! j | j <- goods] + sum [tm0 l M.! j | j <- goods])
    -- taud      =  Td0/sum(h, FF(h));
    taud'   = td0 l / sum [ff l M.! h | h <- factors]

------------------------------------------------------------------
-- * The calibration result
------------------------------------------------------------------

-- | Benchmark levels + calibrated parameters, bundled — the full \"InitVar\"
-- payload task 1a owes @CGELite.CGEParams@ (levels are carried too because
-- task 1c compares the solved allocation back against them).
data Calibration = Calibration
    { calLevels0 :: !Levels0
    , calParams  :: !Params
    } deriving (Eq, Show)

-- | The one calibration of the Hosoe Ch.6 SAM. Pure data: safe to share.
calibration :: Calibration
calibration = Calibration { calLevels0 = levels0, calParams = params levels0 }

-- | The benchmark utility level @UU = prod(i, Xp0(i)**alpha(i))@ — the GAMS
-- objective evaluated at the benchmark consumption bundle. Must equal the
-- @UU@ row of @GAMS\/results.csv@ (25.5085..., checked by the test); the
-- solved model reproducing /this/ number is task 1c's headline sentinel.
benchmarkUtility :: Calibration -> Double
benchmarkUtility (Calibration l p) =
    product [xp0 l M.! i ** (alpha p M.! i) | i <- goods]
