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
import qualified Graphics.Gnuplot.Private.Graph as Graph

import Graphics.Gnuplot.Private.FrameOptionSet (T, )

import Graphics.Gnuplot.Utility
   (quote, )


size :: Graph.C graph => Double -> Double -> T graph -> T graph
size x y =
   OptionSet.add Option.size [show x ++ ", " ++ show y]

title :: Graph.C graph => String -> T graph -> T graph
title text =
   OptionSet.add Option.title [quote text]


xRange :: Graph.C graph => Double -> Double -> T graph -> T graph
xRange = range Option.xRange

yRange :: Graph.C graph => Double -> Double -> T graph -> T graph
yRange = range Option.yRange

zRange :: Double -> Double -> T Graph3D.T -> T Graph3D.T
zRange = range Option.zRange

range :: Graph.C graph => Option.T -> Double -> Double -> T graph -> T graph
range opt x y =
   OptionSet.add opt [show x ++ ":" ++ show y]


xLabel :: Graph.C graph => String -> T graph -> T graph
xLabel = label Option.xLabel

yLabel :: Graph.C graph => String -> T graph -> T graph
yLabel = label Option.yLabel

zLabel :: String -> T Graph3D.T -> T Graph3D.T
zLabel = label Option.zLabel

label :: Graph.C graph => Option.T -> String -> T graph -> T graph
label opt x =
   OptionSet.add opt [quote x]


{-
xTicks :: Graph.C graph => Double -> Double -> T graph -> T graph
xTicks = ticks Option.xTicks

yTicks :: Graph.C graph => Double -> Double -> T graph -> T graph
yTicks = ticks Option.yTicks

zTicks :: Double -> Double -> T Graph3D.T -> T Graph3D.T
zTicks = ticks Option.zTicks

ticks :: Graph.C graph => Option.T -> Double -> Double -> T graph -> T graph
ticks opt x y =
   OptionSet.add opt [show x ++ ":" ++ show y]
-}

{-
cornerToColor :: CornersToColor -> T Graph3D.T -> T Graph3D.T

type3d :: Plot3dType -> T Graph3D.T -> T Graph3D.T
-}
