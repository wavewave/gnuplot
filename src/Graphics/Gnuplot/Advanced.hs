{-
The plot functions here return ExitCode,
which is nice for programming
but ugly for interactive GHCi sessions.
-}
module Graphics.Gnuplot.Advanced (
    plot,
    plotMulti,
  ) where

import qualified Graphics.Gnuplot.Private.Graph as Graph
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


import Data.Array (Array, elems, bounds, )
import Data.Ix (Ix, rangeSize, )
import System.Exit (ExitCode, )
import Graphics.Gnuplot.Utility (quote, commaConcat, )
import qualified Data.Monoid.State as State
import Data.Monoid (mconcat, )
import Data.List (intersperse, mapAccumL, )


-- * User front-end


plot ::
   (Terminal.C terminal, Graph.C graph) =>
   terminal -> Frame.T graph -> IO ExitCode
plot term frame@(Frame.Cons options (Plot.Cons mp)) =
   let files = State.evaluate 0 mp
   in  do mapM_ Plot.writeData files
          callGnuplot
             (formatTerminal term :
              OptionSet.diffToString OptionSet.deflt options)
              (plotCmd frame undefined) $
             concatMap (\(Plot.File filename _ grs) ->
                map (\gr -> quote filename ++ " " ++ Graph.toString gr) grs) $
             files

{- |
Experimental. We have to handle options of multiplots, too.
-}
plotMulti ::
   (Terminal.C terminal, Graph.C graph, Ix i, Ix j) =>
   terminal -> Array (i,j) (Frame.T graph) -> IO ExitCode
plotMulti term arr =
   let -- parts :: [(OptionSet.T graph, [Plot.File graph])]
       parts =
          State.evaluate 0 $ mconcat $
          map
             (\(Frame.Cons options (Plot.Cons mp)) ->
                fmap (\p -> [(options,p)]) mp) $
          elems arr
       blocks :: [[String]]
       blocks =
          snd $
          mapAccumL
             (\oldOpts (newOpts, files) ->
                (newOpts,
                 OptionSet.diffToString oldOpts newOpts ++
                 [plotCmd (head $ elems arr) undefined ++ " " ++
                  commaConcat
                     (concatMap (\(Plot.File filename _ grs) ->
                        map (\gr -> quote filename ++ " " ++ Graph.toString gr) grs) $
                      files)]))
             OptionSet.deflt parts
   in  do mapM_ Plot.writeData $ concatMap snd parts
          Exec.simple
             (formatTerminal term :
              (let ((r0,c0), (r1,c1)) = bounds arr
               in  "set multiplot layout " ++
                      show (rangeSize (r0,r1)) ++ ", " ++
                      show (rangeSize (c0,c1))) :
              (concat $ intersperse [] blocks) ++
              ["unset multiplot"])
             ["-persist"]

formatTerminal ::
   (Terminal.C terminal) =>
   terminal -> String
formatTerminal term =
   let (Terminal.Cons options commands) = Terminal.canonical term
   in  concat $ intersperse "; " $
          ("set terminal " ++ unwords options) : commands

plotCmd ::
   Graph.C graph =>
   Frame.T graph -> graph -> String
plotCmd _frame = Graph.command


--------------

callGnuplot :: [String] -> String -> [String] -> IO ExitCode
callGnuplot preamble cmd params =
   Exec.simple
      (preamble ++
       [cmd ++ " " ++ commaConcat params])
      ["-persist"]
   -- instead of the option, one can also use 'set terminal x11 persist'
