module Graphics.Gnuplot.Frame.Option (
   Option.T,
   module Graphics.Gnuplot.Private.FrameOption,
   ) where

-- exclude Cons constructor
-- this does not work, Cons is exported anyway
import Graphics.Gnuplot.Private.FrameOption hiding (T)
import qualified Graphics.Gnuplot.Private.FrameOption as Option
