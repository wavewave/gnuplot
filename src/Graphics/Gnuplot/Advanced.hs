{- |
Modularized interface to gnuplot
that allows complex graphics and fine control of their components.
It is designed for non-interactive use,
e.g. scripts for plotting statistics.

The hierarchy of objects is as follows:

* @Graph2D@: A curve like a sine curve.
  Attributes of a graph are line type, thickness, color.

* @Graph3D@: A surface.

* @Plot@: An overlay of many curves.
  It is parametrized by the graph type (2D or 3D)
  in order to make sure,
  that only graphs of one type are overlayed
  and only according attributes can be accessed.
  You cannot generate plots or graphs alone,
  you can only generate plots containing graphs
  using the functions in "Graphics.Gnuplot.Plot.TwoDimensional"
  and "Graphics.Gnuplot.Plot.ThreeDimensional".
  You can combine plots using the 'Monoid' type class.

* @Frame@: Add options to a plot
  such as border, legend, title, label attributes.
  See "Graphics.Gnuplot.Frame" and "Graphics.Gnuplot.Frame.OptionSet".

* @MultiPlot@: Arrange several frames in a matrix layout.
  See "Graphics.Gnuplot.MultiPlot".


Although the Haskell wrapper shall save you from the burden
of learning gnuplot script syntax,
it happens frequently that people ask,
how to express a certain gnuplot script using this package.
Thus let's annotate the gnuplot script generated by @Demo.multiplot2d@
in order to show, what belongs to where:

> # the terminal selection is part of the 'plot' command of this module
> set terminal x11
> # multiplot initialization belongs to MultiPlot - of course
> set multiplot layout 3, 5
> # hiding the names of the temporary files is a FrameOption
> unset key
> set xrange [-1.0:1.0]
> # this plot contains only one graph,
> # but several graphs could be given separated by commas
> plot "curve0.csv" using 1:2 with lines
> plot "curve1.csv" using 1:2 with lines
> plot "curve2.csv" using 1:2 with lines
> plot "curve3.csv" using 1:2 with lines
> plot "curve4.csv" using 1:2 with lines
> plot "curve5.csv" using 1:2 with lines
> plot "curve6.csv" using 1:2 with lines
> set xrange [-2.5:2.5]
> set yrange [-2.5:2.5]
> # this is a plot build from a Graph3D
> splot "curve7.csv" using 1:2:3 with pm3d
> set xrange [-1.0:1.0]
> set yrange [*:*]
> plot "curve8.csv" using 1:2 with lines
> plot "curve9.csv" using 1:2 with lines
> plot "curve10.csv" using 1:2 with lines
> plot "curve11.csv" using 1:2 with lines
> plot "curve12.csv" using 1:2 with lines
> plot "curve13.csv" using 1:2 with lines
> plot "curve14.csv" using 1:2 with lines
> unset multiplot

-}
module Graphics.Gnuplot.Advanced (
    plot,
    plotDefault,
    plotSync,
    plotAsync,
  ) where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.Display as Display
import qualified Graphics.Gnuplot.Private.Terminal as Terminal
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import qualified Graphics.Gnuplot.Private.Command as Cmd
import Control.Concurrent (ThreadId, forkIO, )
import System.Exit (ExitCode, )

import qualified Data.Monoid.Reader as Reader
import qualified Data.Monoid.State as State
import Data.Monoid (Monoid, mempty, )
import Control.Functor.HT (void, )


-- * User front-end


{- |
The plot function returns 'ExitCode',
which is nice for programming
but ugly for interactive GHCi sessions.
For interactive sessions,
better use "Graphics.Gnuplot.Simple".
@gfx@ must be one of the types @Plot@, @Frame@, @MultiPlot@.
-}
plot ::
   (Terminal.C terminal, Display.C gfx) =>
   terminal -> gfx -> IO ExitCode
plot = plotSync

plotAsync ::
   (Terminal.C terminal, Display.C gfx) =>
   terminal -> gfx -> IO ThreadId
plotAsync term gfx = forkIO $ void $ plotSync term gfx

plotSync ::
   (Terminal.C terminal, Display.C gfx) =>
   terminal -> gfx -> IO ExitCode
plotSync = plotCore . Terminal.canonical

plotCore ::
   (Display.C gfx) =>
   Terminal.T -> gfx -> IO ExitCode
plotCore term gfx =
   Cmd.run $ \dir ->
      let body =
             flip Reader.run dir $
             State.evaluate (0, OptionSet.initial) $
             Display.runScript $
             Display.toScript gfx
      in  (Display.files body,
           Terminal.format term ++ Display.commands body)

{- |
Plot using the default gnuplot terminal.
-}
plotDefault ::
   (Display.C gfx) =>
   gfx -> IO ExitCode
plotDefault =
   plot DefaultTerm.cons

{-
In the module introduction we refer to Monoid.
That is we must import Monoid module in order to make Haddock happy.
On the other hand we do not use Monoid in the module body,
thus GHC emits a warning.
This dummy declaration makes both GHC and Haddock happy.
-}
_haddockDummy :: Monoid a => a
_haddockDummy = mempty
