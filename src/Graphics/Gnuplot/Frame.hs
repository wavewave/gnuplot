module Graphics.Gnuplot.Frame (
   Frame.T,
   cons, simple, empty,
   ) where

import qualified Graphics.Gnuplot.Private.Frame as Frame
import qualified Graphics.Gnuplot.Private.Plot as Plot
import qualified Graphics.Gnuplot.Private.FrameOptionSet as Option
import qualified Graphics.Gnuplot.Private.GraphEmpty as Empty

import qualified Data.Monoid.State as State


cons :: Option.T graph -> Plot.T graph -> Frame.T graph
cons = Frame.Cons

simple :: Plot.T graph -> Frame.T graph
simple = cons Option.deflt

empty :: Frame.T Empty.T
empty =
   simple (Plot.Cons (State.pure []))
