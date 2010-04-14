module Graphics.Gnuplot.Private.Graph2D where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph2DType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Data.List as List

import Prelude hiding (lines, )


data T x y =
   Cons {
      column_   :: Column,
      type_     :: Type,
      lineSpec_ :: LineSpec.T
   }

type Column = [Int]

type Type = String


columnToString :: Column -> String
columnToString =
   concat . List.intersperse ":" . map show

toString :: T x y -> String
toString (Cons c t l) =
   "using " ++ columnToString c ++
   " with " ++ t ++
   " " ++ LineSpec.toString l

instance Graph.C (T x y) where
   command _ = "plot"
   toString = toString


deflt :: GraphType.T x y a -> Column -> T x y
deflt t c = Cons c (GraphType.toString t) LineSpec.deflt

typ :: Type -> T x y -> T x y
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T x y -> T x y
lineSpec ls gr = gr{lineSpec_ = ls}
