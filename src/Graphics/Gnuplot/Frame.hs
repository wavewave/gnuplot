module Graphics.Gnuplot.Frame (
   Frame.T,
   cons, simple,
   ) where

import qualified Graphics.Gnuplot.Private.Frame as Frame
import qualified Graphics.Gnuplot.Private.Plot as Plot
import qualified Graphics.Gnuplot.Private.FrameOptionSet as Option


cons :: Option.T graph -> Plot.T graph -> Frame.T graph
cons = Frame.Cons

simple :: Plot.T graph -> Frame.T graph
simple = cons Option.deflt
