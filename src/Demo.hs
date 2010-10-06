module Main where

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Data.Time as Time

import qualified Paths_gnuplot as Path
import System.FilePath ((</>), )

import Data.Array (listArray, )
import Data.Monoid (mappend, )


simple2d :: Plot2D.T Double Double
simple2d =
   Plot2D.function Graph2D.lines
      (linearScale 100 (-10,10)) sin

circle2d :: Plot2D.T Double Double
circle2d =
   Plot2D.parameterFunction Graph2D.lines
      (linearScale 24 (-pi,pi))
      (\t -> (cos t, sin t))

list2d :: Plot2D.T Int Integer
list2d =
   Plot2D.list Graph2D.listPoints [0,1,1,2,3,5,8,13::Integer]

candle2d :: Plot2D.T Time.Day Double
candle2d =
   Plot2D.list Graph2D.candleSticks $
   zip [(Time.fromGregorian 2008 01 01) ..] $
   flip map (linearScale 32 (0,2*pi)) $
   \t -> (-sin t, -2*sin t, 2*sin t, sin t)

overlay2d :: Frame.T (Graph2D.T Double Double)
overlay2d =
   Frame.cons (Opts.size 1 0.4 $ Opts.remove Opt.key $ Opts.deflt) $
   Plot2D.function Graph2D.lines (linearScale 100 (-pi,pi)) cos
   `mappend`
   circle2d

file2d :: FilePath -> FilePath -> Frame.T (Graph2D.T Int Double)
file2d path file =
   let lineSpec =
          Graph2D.lineSpec $
          LineSpec.title "runtimes" $
          LineSpec.lineWidth 2.5 $
          LineSpec.deflt
       frameOpts =
          Opts.xLabel "input size" $
          Opts.yLabel "runtime (ms)" $
          Opts.title ("Graph from " ++ file) $
          Opts.deflt
   in  Frame.cons frameOpts $ fmap lineSpec $
       Plot2D.pathFromFile Graph2D.lines (path </> file) 1 2

mixed2d :: MultiPlot.T
mixed2d =
   MultiPlot.simpleFromPartArray $
   listArray ((0::Int,0::Int), (0,2)) $
   MultiPlot.partFromPlot circle2d :
   MultiPlot.partFromFrame
      (Frame.cons (Opts.xFormat "%m-%d" $ Opts.remove Opt.key $ Opts.deflt) $
       candle2d) :
   MultiPlot.partFromPlot list2d :
   []

{-
The size settings of overlay2d interfer badly with the other plots,
because 'unset' does not restore the size to the multiplot settings.
-}
size2d :: MultiPlot.T
size2d =
   MultiPlot.simpleFromPartArray $
   listArray ((0::Int,0::Int), (0,2)) $
   MultiPlot.partFromPlot candle2d :
   MultiPlot.partFromFrame
      (Frame.cons (Opts.yLabel "Fibonacci" $ Opts.remove Opt.key $ Opts.deflt)
       list2d) :
   MultiPlot.partFromPlot candle2d :
   []

defltOpts :: Graph.C graph => Opts.T graph
defltOpts =
   Opts.remove Opt.key $
   Opts.deflt

wave3d :: Frame.T (Graph3D.T Double Double Double)
wave3d =
   let meshNodes = linearScale 20 (-2,2)
   in  Frame.cons
          (Opts.xRange3d (-2.5,2.5) $
           Opts.yRange3d (-2.5,2.5) $
           defltOpts) $
       Plot3D.surface
          meshNodes meshNodes
          (\x y -> cos(x*x+y*y))

multiplot :: MultiPlot.T
multiplot =
   let (prefix,suffix) =
          splitAt 7 $
          map MultiPlot.partFromFrame $
          map (\k ->
             Frame.cons (Opts.xRange2d (-1,1) defltOpts) $
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
main = sequence_ $
   Plot.plot X11.cons simple2d :
   Plot.plot X11.cons list2d :
   Plot.plot X11.cons candle2d :
   Plot.plot X11.cons overlay2d :
   (Plot.plot X11.cons . flip file2d "runtime.data"
      =<< fmap (</> "data") Path.getDataDir) :
   Plot.plot X11.cons mixed2d :
   Plot.plot X11.cons size2d :
   Plot.plot X11.cons wave3d :
   Plot.plot X11.cons multiplot :
   []
