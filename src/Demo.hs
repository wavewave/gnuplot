module Main where

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import Data.Monoid (mappend, )


simple2d :: Frame.T Graph2D.T
simple2d =
   Frame.simple $
   Plot2D.function (linearScale 100 (-10,10::Double)) sin

overlay2d :: Frame.T Graph2D.T
overlay2d =
   Frame.cons (Opts.size 1 0.4 $ Opts.remove Opt.key $ Opts.deflt) $
   Plot2D.function (linearScale 100 (-pi,pi::Double)) cos
   `mappend`
   fmap
      (Graph2D.typ Graph2D.points)
      (Plot2D.parameterFunction
         (linearScale 24 (-pi,pi::Double))
         (\t -> (cos t, sin t)))


main :: IO ()
main =
   do Plot.plot X11.cons simple2d
      Plot.plot X11.cons overlay2d
      return ()
