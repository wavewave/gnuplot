module Main where

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import Data.Array (listArray, )
import Data.Monoid (mappend, )


simple2d :: Plot2D.T
simple2d =
   Plot2D.function Graph2D.lines
      (linearScale 100 (-10,10::Double)) sin

circle2d :: Plot2D.T
circle2d =
   Plot2D.parameterFunction Graph2D.lines
      (linearScale 24 (-pi,pi::Double))
      (\t -> (cos t, sin t))

list2d :: Plot2D.T
list2d =
   Plot2D.list Graph2D.listPoints [0,1,1,2,3,5,8,13::Integer]

candle2d :: Plot2D.T
candle2d =
   Plot2D.list Graph2D.candleSticks $
   Plot2D.functionToGraph (linearScale 32 (0,2*pi::Double)) $
   \t -> (-sin t, -2*sin t, 2*sin t, sin t)

overlay2d :: Frame.T Graph2D.T
overlay2d =
   Frame.cons (Opts.size 1 0.4 $ Opts.remove Opt.key $ Opts.deflt) $
   Plot2D.function Graph2D.lines (linearScale 100 (-pi,pi::Double)) cos
   `mappend`
   circle2d

defltOpts :: Opts.T graph
defltOpts =
   Opts.remove Opt.key $
   Opts.deflt

wave3d :: Frame.T Graph3D.T
wave3d =
   let meshNodes = linearScale 20 (-2,2::Double)
   in  Frame.cons
          (Opts.xRange (-2.5,2.5) $
           Opts.yRange (-2.5,2.5) $
           defltOpts) $
       Plot3D.surface
          meshNodes meshNodes
          (\x y -> cos(x*x+y*y))

multiplot2d :: MultiPlot.T
multiplot2d =
   let opts :: Opts.T graph
       opts =
          Opts.remove Opt.key $
          Opts.deflt
       (prefix,suffix) =
          splitAt 7 $
          map MultiPlot.partFromFrame $
          map (\k ->
             Frame.cons (Opts.xRange (-1,1) opts) $
             Plot2D.parameterFunction Graph2D.lines
                (linearScale 48 (-pi,pi::Double))
                (\t -> (cos (t + pi/7*fromInteger k), sin (2*t)))) $
          [0..13]
{-
       center =
          MultiPlot.partFromFrame Frame.empty
-}
       center =
          MultiPlot.partFromFrame wave3d
   in  MultiPlot.simpleFromPartArray $
       listArray ((0::Int,0::Int), (2,4)) $
       prefix ++ center : suffix


main :: IO ()
main =
   do Plot.plot X11.cons simple2d
      Plot.plot X11.cons list2d
      Plot.plot X11.cons candle2d
      Plot.plot X11.cons overlay2d
      Plot.plot X11.cons wave3d
      Plot.plot X11.cons multiplot2d
      return ()
