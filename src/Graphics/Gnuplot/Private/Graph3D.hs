module Graphics.Gnuplot.Private.Graph3D where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph3DType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph

import Graphics.Gnuplot.Private.Graph2D (Column, columnToString, )

import Prelude hiding (lines, )


data T =
   Cons {
      column_   :: Column,
      type_     :: Type,
      lineSpec_ :: LineSpec.T
   }

type Type = String


toString :: T -> String
toString (Cons c t l) =
   "using " ++ columnToString c ++
   " with " ++ t ++
   " " ++ LineSpec.toString l

instance Graph.C T where
   command _ = "splot"
   toString = toString


deflt :: GraphType.T a -> Column -> T
deflt t c = Cons c (GraphType.toString t) LineSpec.deflt

typ :: Type -> T -> T
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T -> T
lineSpec ls gr = gr{lineSpec_ = ls}
