module Graphics.Gnuplot.Plot where

import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Private.Plot  as Plot

import Graphics.Gnuplot.Utility
   (functionToGraph, )

import qualified Data.List.Match as Match


-- * computed plots

type Plot = Plot.T Graph.T

{- |
> list (take 30 (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs))
-}
list :: Show a => [a] -> Plot
list dat =
   Plot.withUniqueFile
      (unlines (map show dat))
      ([Graph.deflt (Graph.Dim1 1)])

{- |
> function (linearScale 1000 (-10,10)) sin
-}
function :: Show a => [a] -> (a -> a) -> Plot
function args f =
   path (functionToGraph args f)

{- |
> functions (linearScale 1000 (-10,10)) [sin, cos]
-}
functions :: Show a => [a] -> [a -> a] -> Plot
functions args fs =
   let dat = map (\x -> (x, map ($ x) fs)) args
   in  Plot.withUniqueFile
          (unlines (map (unwords . map show . uncurry (:)) dat))
          (Match.take fs $
           map (Graph.deflt . Graph.Dim2 1) [2..])

path :: Show a => [(a,a)] -> Plot
path dat =
   Plot.withUniqueFile
      (unlines (map (\(x,y) -> show x ++ " " ++ show y) dat))
      ([Graph.deflt (Graph.Dim2 1 2)])

{- |
> parameterFunction (linearScale 1000 (0,2*pi)) (\t -> (sin (2*t), cos t))
-}
parameterFunction :: Show a => [a] -> (a -> (a,a)) -> Plot
parameterFunction args f = path (map f args)



-- * plot stored data

listFromFile :: FilePath -> Int -> Plot
listFromFile filename column =
   Plot.fromGraphs filename [Graph.deflt (Graph.Dim1 column)]

pathFromFile :: FilePath -> Int -> Int -> Plot
pathFromFile filename columnX columnY =
   Plot.fromGraphs filename [Graph.deflt (Graph.Dim2 columnX columnY)]
