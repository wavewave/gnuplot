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
  See "Graphics.Gnuplot.Frame".

* @MultiPlot@: Arrange several frames in a matrix layout.
  See "Graphics.Gnuplot.MultiPlot".
-}
module Graphics.Gnuplot.Advanced (
    plot,
  ) where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Display as Display

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Execute as Exec

import System.Exit (ExitCode, )
import Data.Monoid (Monoid, ) -- for Haddock
import qualified Data.Monoid.State as State
import Data.List (intersperse, )


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
plot term gfx =
   let body =
          State.evaluate (0, OptionSet.decons OptionSet.deflt) $
          Display.runScript $
          Display.toScript gfx
   in  do mapM_ Display.writeData (Display.files body)
          Exec.simple
             (formatTerminal term : Display.commands body)
             ["-persist"]

formatTerminal ::
   (Terminal.C terminal) =>
   terminal -> String
formatTerminal term =
   let (Terminal.Cons options commands) = Terminal.canonical term
   in  concat $ intersperse "; " $
          ("set terminal " ++ unwords options) : commands
