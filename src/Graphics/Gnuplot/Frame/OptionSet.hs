module Graphics.Gnuplot.Frame.OptionSet (
   OptionSet.T,
   deflt,

   OptionSet.add,
   OptionSet.remove,

   size,
   title,
   xRange2d,
   yRange2d,
   xRange3d,
   yRange3d,
   zRange3d,
   xLabel,
   yLabel,
   zLabel,
   xFormat,
   yFormat,
   zFormat,

   view,
   viewMap,
   ) where


import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.FrameOption as Option
import qualified Graphics.Gnuplot.Private.Graph as Graph

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import Graphics.Gnuplot.Private.FrameOptionSet (T, )

import Graphics.Gnuplot.Utility
   (quote, )


deflt :: Graph.C graph => T graph
deflt = Graph.defltOptions


size :: Graph.C graph => Double -> Double -> T graph -> T graph
size x y =
   OptionSet.add Option.size [show x ++ ", " ++ show y]

title :: Graph.C graph => String -> T graph -> T graph
title text =
   OptionSet.add Option.title [quote text]


{-
xRange :: Graph.C graph => (Double, Double) -> T graph -> T graph
xRange = range Option.xRange

yRange :: Graph.C graph => (Double, Double) -> T graph -> T graph
yRange = range Option.yRange

zRange :: (Double, Double) -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zRange = range Option.zRange
-}

xRange2d ::
   (Atom.C x, Atom.C y, Tuple.C x) =>
   (x, x) -> T (Graph2D.T x y) -> T (Graph2D.T x y)
xRange2d = range Option.xRange

yRange2d ::
   (Atom.C x, Atom.C y, Tuple.C y) =>
   (y, y) -> T (Graph2D.T x y) -> T (Graph2D.T x y)
yRange2d = range Option.yRange


xRange3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C x) =>
   (x, x) -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
xRange3d = range Option.xRange

yRange3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C y) =>
   (y, y) -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
yRange3d = range Option.yRange

zRange3d ::
   (Atom.C x, Atom.C y, Atom.C z, Tuple.C z) =>
   (z, z) -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zRange3d = range Option.zRange


range ::
   (Atom.C a, Tuple.C a, Graph.C graph) =>
   Option.T -> (a, a) -> T graph -> T graph
range opt (x,y) =
   OptionSet.add opt
      [showString "[" . atomText x .
       showString ":" . atomText y $
       "]"]

atomText ::
   (Atom.C a, Tuple.C a) =>
   a -> ShowS
atomText x =
   case Tuple.text x of
      [s] -> s
      _ -> error "OptionSet.fromSingleton: types of Atom class must generate single representation texts"


xLabel :: Graph.C graph => String -> T graph -> T graph
xLabel = label Option.xLabel

yLabel :: Graph.C graph => String -> T graph -> T graph
yLabel = label Option.yLabel

zLabel ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   String -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zLabel = label Option.zLabel

label :: Graph.C graph => Option.T -> String -> T graph -> T graph
label opt x =
   OptionSet.add opt [quote x]


xFormat :: Graph.C graph => String -> T graph -> T graph
xFormat = format Option.xFormat

yFormat :: Graph.C graph => String -> T graph -> T graph
yFormat = format Option.yFormat

zFormat ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   String -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zFormat = format Option.zFormat

format :: Graph.C graph => Option.T -> String -> T graph -> T graph
format opt x =
   OptionSet.add opt [quote x]


{- |
Set parameters of viewing a surface graph.
See <info:gnuplot/view>
-}
view ::
   Double {- ^ rotateX -} ->
   Double {- ^ rotateZ -} ->
   Double {- ^ scale -} ->
   Double {- ^ scaleZ -} ->
   T (Graph3D.T x y z) -> T (Graph3D.T x y z)
view rotateX rotateZ scale scaleZ =
   OptionSet.add Option.view [show rotateX, show rotateZ, show scale, show scaleZ]

{- |
Show flat pixel map.
-}
viewMap :: T (Graph3D.T x y z) -> T (Graph3D.T x y z)
viewMap =
   OptionSet.add Option.view ["map"]

{-
xTicks :: Graph.C graph => Double -> Double -> T graph -> T graph
xTicks = ticks Option.xTicks

yTicks :: Graph.C graph => Double -> Double -> T graph -> T graph
yTicks = ticks Option.yTicks

zTicks :: Double -> Double -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
zTicks = ticks Option.zTicks

ticks :: Graph.C graph => Option.T -> Double -> Double -> T graph -> T graph
ticks opt x y =
   OptionSet.add opt [show x ++ ":" ++ show y]
-}

{-
cornerToColor :: CornersToColor -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)

type3d :: Plot3dType -> T (Graph3D.T x y z) -> T (Graph3D.T x y z)
-}
