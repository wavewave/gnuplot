module Graphics.Gnuplot.Private.Frame where

import qualified Graphics.Gnuplot.Private.Plot as Plot
import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet

import qualified Graphics.Gnuplot.Display as Display
import qualified Graphics.Gnuplot.Private.Graph as Graph

import qualified Data.Monoid.State as State
import Data.Monoid (Monoid, mappend, )


data T graph =
   Cons {
      option :: OptionSet.T graph,
      plot :: Plot.T graph
   }

instance Graph.C graph => Display.C (T graph) where
   toScript frame =
      (Display.Script $
         State.Cons $ \(n, opts0) ->
            let opts1 = option frame
            in  (Display.Body [] $
                 OptionSet.diffToString (OptionSet.Cons opts0) opts1,
                 (n, OptionSet.decons opts1)))
      `mappend`
      (Display.toScript $ plot frame)
