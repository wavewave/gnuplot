module Graphics.Gnuplot.Private.Frame where

import qualified Graphics.Gnuplot.Private.Plot as Plot
import qualified Graphics.Gnuplot.Private.FrameOptionSet as Option

data T graph =
   Cons {
      option :: Option.T graph,
      plot :: Plot.T graph
   }
