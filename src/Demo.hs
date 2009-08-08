module Main where

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import Data.Array (Array, listArray, )
import Data.Monoid (mappend, )


simple2d :: Plot2D.T
simple2d =
   Plot2D.function (linearScale 100 (-10,10::Double)) sin

circle2d :: Plot2D.T
circle2d =
   fmap
      (Graph2D.typ Graph2D.points)
      (Plot2D.parameterFunction
         (linearScale 24 (-pi,pi::Double))
         (\t -> (cos t, sin t)))

overlay2d :: Frame.T Graph2D.T
overlay2d =
   Frame.cons (Opts.size 1 0.4 $ Opts.remove Opt.key $ Opts.deflt) $
   Plot2D.function (linearScale 100 (-pi,pi::Double)) cos
   `mappend`
   circle2d

multiplot2d :: Array (Int,Int) (Frame.T Graph2D.T)
multiplot2d =
   let opts =
          Opts.remove Opt.key $
          Opts.deflt
       (prefix,suffix) =
          splitAt 7 $
          map (\k ->
             Frame.cons opts $
             Plot2D.parameterFunction
                (linearScale 48 (-pi,pi::Double))
                (\t -> (cos (t + pi/7*fromInteger k), sin (2*t)))) $
          [0..13]
       center =
          Frame.cons
             (Opts.xRange (-1.5,1.5) $
              Opts.yRange (-1.5,1.5) $
              opts) $
          circle2d
          `mappend`
          (Plot2D.parameterFunction
             (linearScale 24 (-pi,pi::Double))
             (\t -> (0.7*cos t, 0.7*sin t)))
   in  listArray ((0,0), (2,4)) $
       prefix ++ center : suffix


main :: IO ()
main =
   do Plot.plot X11.cons simple2d
      Plot.plot X11.cons overlay2d
      Plot.plotMulti X11.cons multiplot2d
      return ()
