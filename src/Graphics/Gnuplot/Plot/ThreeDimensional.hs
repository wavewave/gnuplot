module Graphics.Gnuplot.Plot.ThreeDimensional (
   T,
   cloud,
--   function,
   mesh,
   surface,

   linearScale,
   functionToGraph,
   ) where

import qualified Graphics.Gnuplot.Private.Graph3DType as Type
import qualified Graphics.Gnuplot.Private.Graph3D as Graph
import qualified Graphics.Gnuplot.Private.Plot    as Plot
-- import qualified Graphics.Gnuplot.Value.ColumnSet as Col
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom  as Atom

import Graphics.Gnuplot.Utility
   (functionToGraph, linearScale, assembleCells, )

import Data.List.HT (outerProduct, )


{- |
Plots can be assembled using 'mappend' or 'mconcat'.
-}
type T = Plot.T Graph.T


-- * computed plots

cloud ::
   (Tuple.C a) =>
   Type.T a -> [a] -> T
cloud typ ps =
   Plot.withUniqueFile
      (assembleCells (map Tuple.text ps))
      [Graph.deflt typ [1 .. Type.tupleSize typ]]

{-
function ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   Type.T (x,y,z) -> [x] -> [y] -> (x -> y -> z) -> T
function xArgs yArgs f =
   cloud (liftM2 (\x y -> (x, y, f x y)) xArgs yArgs)
-}


mesh ::
   (Atom.C x, Atom.C y, Atom.C z,
    Tuple.C x, Tuple.C y, Tuple.C z) =>
   [[(x,y,z)]] -> T
mesh pss =
   let typ :: [[a]] -> Type.T a -> Type.T a
       typ _ = id
   in  Plot.withUniqueFile
          (assembleCells (concat (map (\ps -> map Tuple.text ps ++ [[]]) pss)))
          [Graph.deflt (typ pss Type.pm3d) (1:2:3:[])]

surface ::
   (Atom.C x, Atom.C y, Atom.C z,
    Tuple.C x, Tuple.C y, Tuple.C z) =>
   [x] -> [y] -> (x -> y -> z) -> T
surface xArgs yArgs f =
   mesh (outerProduct (\x y -> (x, y, f x y)) xArgs yArgs)
