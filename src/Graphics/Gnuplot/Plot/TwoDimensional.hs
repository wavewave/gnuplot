-- shall this also export Graph.Type, set plotType and so on?
module Graphics.Gnuplot.Plot.TwoDimensional (
   T,

   list,
   function,
   functions,
   parameterFunction,

   listFromFile,
   pathFromFile,

   linearScale,
   functionToGraph,
   ) where

import qualified Graphics.Gnuplot.Private.Graph2DType as Type
import qualified Graphics.Gnuplot.Private.Graph2D as Graph
import qualified Graphics.Gnuplot.Private.Plot    as Plot
import qualified Graphics.Gnuplot.Value.ColumnSet as Col
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom  as Atom

import Graphics.Gnuplot.Utility
   (functionToGraph, linearScale, assembleCells, )

import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT


{- |
Plots can be assembled using 'mappend' or 'mconcat'.
-}
type T = Plot.T Graph.T


-- * computed plots

{- |
> list Type.listLines (take 30 (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs))
> list Type.lines (take 30 (let fibs0 = 0 : fibs1; fibs1 = 1 : zipWith (+) fibs0 fibs1 in zip fibs0 fibs1))
-}
list :: Tuple.C a => Type.T a -> [a] -> T
list typ ps =
   Plot.withUniqueFile
      (assembleCells (map Tuple.text ps))
      [Graph.deflt typ [1 .. Type.tupleSize typ]]

{- |
> function Type.line (linearScale 1000 (-10,10)) sin
-}
function :: (Tuple.C x, Tuple.C y) => Type.T (x,y) -> [x] -> (x -> y) -> T
function typ args f =
   list typ (functionToGraph args f)

{- |
> functions Type.line (linearScale 1000 (-10,10)) [sin, cos]
-}
functions :: (Tuple.C x, Tuple.C y) => Type.T (x,y) -> [x] -> [x -> y] -> T
functions typ args fs =
   let dat = map (\x -> (x, map ($ x) fs)) args
       typX :: Type.T (x,y) -> Type.T x
       typX = undefined
       typY :: Type.T (x,y) -> Type.T y
       typY = undefined
       nx = Type.tupleSize (typX typ)
   in  Plot.withUniqueFile
          (assembleCells
             (map (\(x,y) -> Tuple.text x ++ concatMap Tuple.text y) dat))
          (Match.take fs $
           map (\ns -> Graph.deflt typ ([1..nx] ++ ns)) $
           ListHT.sliceVertical (Type.tupleSize (typY typ)) [(nx+1)..])


{- |
> parameterFunction Type.line (linearScale 1000 (0,2*pi)) (\t -> (sin (2*t), cos t))
-}
parameterFunction :: Tuple.C a => Type.T a -> [t] -> (t -> a) -> T
parameterFunction typ args f = list typ (map f args)



-- * plot stored data

fromFile ::
   Type.T y -> FilePath -> Col.T y -> T
fromFile typ filename (Col.Cons cs) =
   Plot.fromGraphs filename [Graph.deflt typ cs]

listFromFile :: Atom.C y =>
   Type.T y -> FilePath -> Int -> T
listFromFile typ filename column =
   fromFile typ filename (Col.atom column)

pathFromFile :: (Atom.C x, Atom.C y) =>
   Type.T (x,y) -> FilePath -> Int -> Int -> T
pathFromFile typ filename columnX columnY =
   fromFile typ filename (Col.pair (Col.atom columnX) (Col.atom columnY))
