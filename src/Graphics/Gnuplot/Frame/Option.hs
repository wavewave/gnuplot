module Graphics.Gnuplot.Frame.Option (
   OptionSet.T,
   module Graphics.Gnuplot.Private.FrameOptionSet,
   ) where

-- exclude Cons constructor
import Graphics.Gnuplot.Private.FrameOptionSet hiding (T)
import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
