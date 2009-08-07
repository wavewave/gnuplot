{-
The plot functions here return ExitCode,
which is nice for programming
but ugly for interactive GHCi sessions.
-}
module Graphics.Gnuplot.Advanced (
    linearScale,

    plot2d,
  ) where

import qualified Graphics.Gnuplot.Private.Graph2D as Graph2D
-- import qualified Graphics.Gnuplot.Private.Graph3D as Graph3D
import qualified Graphics.Gnuplot.Private.Plot as Plot
import qualified Graphics.Gnuplot.Private.Frame as Frame
import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet

{-
import qualified Graphics.Gnuplot.Terminal.PostScript as PS
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
-}

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Execute as Exec


import System.Exit (ExitCode, )
import Graphics.Gnuplot.Utility
   (quote, commaConcat, )
import qualified Data.Monoid.State as State
import Data.List (intersperse, )


-- * User front-end


linearScale :: Fractional a => Integer -> (a,a) -> [a]
linearScale n (x0,x1) =
   map (\m -> x0 + (x1-x0) * fromIntegral m / fromIntegral n) [0..n]



plot2d ::
   (Terminal.C terminal) =>
   terminal -> Frame.T Graph2D.T -> IO ExitCode
plot2d term (Frame.Cons frameOptions (Plot.Cons mp)) =
   let files = State.evaluate 0 mp
   in  do mapM_ Plot.writeData files
          callGnuplot
             ((let (Terminal.Cons options commands) = Terminal.canonical term
               in  concat $ intersperse "; " $
                   ("set terminal " ++ unwords options) : commands) :
              OptionSet.diffToString OptionSet.deflt frameOptions)
                "plot" $
             concatMap (\(Plot.File filename _ grs) ->
                map (\gr -> quote filename ++ " " ++ Graph2D.toString gr) grs) $
             files


--------------

callGnuplot :: [String] -> String -> [String] -> IO ExitCode
callGnuplot preamble cmd params =
   Exec.simple
      (preamble ++
       [cmd ++ " " ++ commaConcat params])
      ["-persist"]
   -- instead of the option, one can also use 'set terminal x11 persist'
