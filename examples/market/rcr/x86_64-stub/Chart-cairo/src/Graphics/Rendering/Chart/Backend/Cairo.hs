{- |
    Module     : Graphics.Rendering.Chart.Backend.Cairo
    Copyright  : (c) Kaya Akagi. 2026
    Maintainer : yakagika@icloud.com

    No-op stand-in for the Chart-cairo file backend, used only by the RCR
    @--x86_64@ route (see @examples/market/rcr/INSTALL.md@). Homebrew no longer
    installs an x86_64 cairo on Apple Silicon. The pinned revisions use this
    module only through 'renderableToFile' in @ExchangeAlgebra.Simulate.Visualize@,
    and no measured program draws a chart.
-}

module Graphics.Rendering.Chart.Backend.Cairo ( FileFormat (..)
                                              , FileOptions (..)
                                              , renderableToFile
                                              ) where

import Data.Default.Class                  (Default (..))
import Graphics.Rendering.Chart.Renderable (PickFn, Renderable)

-- | Output file formats, as in Chart-cairo 1.9.4.1.
data FileFormat
    = PNG
    | SVG
    | PS
    | PDF

-- | Output options, as in Chart-cairo 1.9.4.1.
data FileOptions = FileOptions
    { _fo_size   :: (Int, Int) -- ^ Width and height in pixels.
    , _fo_format :: FileFormat -- ^ Output format.
    }

-- | The Chart-cairo default: 800 by 600 pixels, PNG.
instance Default FileOptions where
    def = FileOptions (800, 600) PNG

-- | Raise an IO error instead of drawing.
--
-- Invariant: no measured program renders a chart, so a call means that a chart
-- was requested from the @--x86_64@ build, which cannot draw.
renderableToFile :: FileOptions -> FilePath -> Renderable a -> IO (PickFn a)
renderableToFile _ path _ = ioError (userError message)
  where
    message = "renderableToFile: the RCR --x86_64 build cannot draw charts: " ++ path
