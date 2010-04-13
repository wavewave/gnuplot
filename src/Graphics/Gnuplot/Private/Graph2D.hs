module Graphics.Gnuplot.Private.Graph2D where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph2DType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Data.List as List

import Prelude hiding (lines, )


data T =
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

toString :: T -> String
toString (Cons c t l) =
   "using " ++ columnToString c ++
   " with " ++ t ++
   " " ++ LineSpec.toString l

instance Graph.C T where
   command _ = "plot"
   toString = toString


deflt :: GraphType.T a -> Column -> T
deflt t c = Cons c (GraphType.toString t) LineSpec.deflt

typ :: Type -> T -> T
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T -> T
lineSpec ls gr = gr{lineSpec_ = ls}
