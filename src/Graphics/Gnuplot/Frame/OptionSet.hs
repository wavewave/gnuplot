module Graphics.Gnuplot.Frame.OptionSet (
   OptionSet.T,
   OptionSet.deflt,

   OptionSet.add,
   OptionSet.remove,

   size,
   title,
   xRange,
   yRange,
   zRange,
   xLabel,
   yLabel,
   zLabel,
   ) where


import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.FrameOption as Option

import Graphics.Gnuplot.Private.FrameOptionSet (T, )

import Graphics.Gnuplot.Utility
   (quote, )


size :: Double -> Double -> T graph -> T graph
size x y =
   OptionSet.add Option.size [show x ++ ", " ++ show y]

title :: String -> T graph -> T graph
title text =
   OptionSet.add Option.title [quote text]


xRange :: Double -> Double -> T graph -> T graph
xRange = range Option.xRange

yRange :: Double -> Double -> T graph -> T graph
yRange = range Option.yRange

zRange :: Double -> Double -> T Graph3D.T -> T Graph3D.T
zRange = range Option.zRange

range :: Option.T -> Double -> Double -> T graph -> T graph
range opt x y =
   OptionSet.add opt [show x ++ ":" ++ show y]


xLabel :: String -> T graph -> T graph
xLabel = label Option.xLabel

yLabel :: String -> T graph -> T graph
yLabel = label Option.yLabel

zLabel :: String -> T Graph3D.T -> T Graph3D.T
zLabel = label Option.zLabel

label :: Option.T -> String -> T graph -> T graph
label opt x =
   OptionSet.add opt [quote x]


{-
xTicks :: Double -> Double -> T graph -> T graph
xTicks = ticks Option.xTicks

yTicks :: Double -> Double -> T graph -> T graph
yTicks = ticks Option.yTicks

zTicks :: Double -> Double -> T Graph3D.T -> T Graph3D.T
zTicks = ticks Option.zTicks

ticks :: Option.T -> Double -> Double -> T graph -> T graph
ticks opt x y =
   OptionSet.add opt [show x ++ ":" ++ show y]
-}

{-
cornerToColor :: CornersToColor -> T Graph3D.T -> T Graph3D.T

type3d :: Plot3dType -> T Graph3D.T -> T Graph3D.T
-}
