-- shall this also export Graph.Type, set plotType and so on?
module Graphics.Gnuplot.Plot.TwoDimensional where

import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Private.Plot  as Plot

import Graphics.Gnuplot.Utility
   (functionToGraph, commaConcat, )

import qualified Data.List.Match as Match


{- |
Plots can be assembled using 'mappend' or 'mconcat'.
-}
type T = Plot.T Graph.T


-- * computed plots

{- |
> list (take 30 (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs))
-}
list :: Show a => [a] -> T
list dat =
   Plot.withUniqueFile
      (unlines (map show dat))
      ([Graph.deflt (Graph.Dim1 1)])

{- |
> function (linearScale 1000 (-10,10)) sin
-}
function :: Show a => [a] -> (a -> a) -> T
function args f =
   path (functionToGraph args f)

{- |
> functions (linearScale 1000 (-10,10)) [sin, cos]
-}
functions :: Show a => [a] -> [a -> a] -> T
functions args fs =
   let dat = map (\x -> (x, map ($ x) fs)) args
   in  Plot.withUniqueFile
          (unlines (map (commaConcat . map show . uncurry (:)) dat))
          (Match.take fs $
           map (Graph.deflt . Graph.Dim2 1) [2..])

path :: Show a => [(a,a)] -> T
path dat =
   Plot.withUniqueFile
      (unlines (map (\(x,y) -> show x ++ ", " ++ show y) dat))
      ([Graph.deflt (Graph.Dim2 1 2)])

{- |
> parameterFunction (linearScale 1000 (0,2*pi)) (\t -> (sin (2*t), cos t))
-}
parameterFunction :: Show a => [a] -> (a -> (a,a)) -> T
parameterFunction args f = path (map f args)



-- * plot stored data

listFromFile :: FilePath -> Int -> T
listFromFile filename column =
   Plot.fromGraphs filename [Graph.deflt (Graph.Dim1 column)]

pathFromFile :: FilePath -> Int -> Int -> T
pathFromFile filename columnX columnY =
   Plot.fromGraphs filename [Graph.deflt (Graph.Dim2 columnX columnY)]
