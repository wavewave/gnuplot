module Graphics.Gnuplot.Private.Graph3D where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph3DType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph

import Graphics.Gnuplot.Private.Graph2D (Column, columnToString, )

import Prelude hiding (lines, )


data T x y z =
   Cons {
      column_   :: Column,
      type_     :: Type,
      lineSpec_ :: LineSpec.T
   }

type Type = String


toString :: T x y z -> String
toString (Cons c t l) =
   "using " ++ columnToString c ++
   " with " ++ t ++
   " " ++ LineSpec.toString l

instance Graph.C (T x y z) where
   command _ = "splot"
   toString = toString


deflt :: GraphType.T x y z a -> Column -> T x y z
deflt t c = Cons c (GraphType.toString t) LineSpec.deflt

typ :: Type -> T x y z -> T x y z
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T x y z -> T x y z
lineSpec ls gr = gr{lineSpec_ = ls}
