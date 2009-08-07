module Graphics.Gnuplot.Frame.Option (
   Option.T,
   module Graphics.Gnuplot.Private.FrameOption,
   ) where

-- exclude Cons constructor
import Graphics.Gnuplot.Private.FrameOption hiding (T)
import qualified Graphics.Gnuplot.Private.FrameOption as Option
