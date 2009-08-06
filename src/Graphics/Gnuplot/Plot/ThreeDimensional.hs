module Graphics.Gnuplot.Plot.ThreeDimensional where

import qualified Graphics.Gnuplot.Private.Graph3D as Graph
import qualified Graphics.Gnuplot.Private.Plot    as Plot

import Graphics.Gnuplot.Utility
   (showTriplet, )

import Data.List.HT (outerProduct, )


{- |
Plots can be assembled using 'mappend' or 'mconcat'.
-}
type T = Plot.T Graph.T


-- * computed plots

mesh ::
   (Show a, Show b, Show c) =>
   [[(a,b,c)]] -> T
mesh dat =
   Plot.withUniqueFile
      (unlines (map (unlines . map showTriplet) dat))
      [Graph.deflt (Graph.Dim3 1 2 3)]

function ::
   (Show a, Show b, Show c) =>
   [b] -> [c] -> (b -> c -> a) -> T
function xArgs yArgs f =
   mesh (outerProduct (\x y -> (x, y, f x y)) xArgs yArgs)
