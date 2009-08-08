{- |
Advanced interface to gnuplot
that allows finer control and more complex graphics.

The plot functions here return 'ExitCode',
which is nice for programming
but ugly for interactive GHCi sessions.
-}
module Graphics.Gnuplot.Advanced (
    plot,
  ) where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Display as Display

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Execute as Exec

import System.Exit (ExitCode, )
import qualified Data.Monoid.State as State
import Data.List (intersperse, )


-- * User front-end


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
