module Graphics.Gnuplot.Private.Graph where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet

class C graph where
   command :: graph -> String
   toString :: graph -> String
   defltOptions :: OptionSet.T graph
