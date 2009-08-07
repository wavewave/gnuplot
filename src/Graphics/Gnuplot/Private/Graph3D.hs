module Graphics.Gnuplot.Private.Graph3D where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.GraphType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph

import Prelude hiding (lines, )


data T =
   Cons {
      column_   :: Column,
      type_     :: Type,
      lineSpec_ :: LineSpec.T
   }

data Column =
     Dim3 {columnX, columnY, columnZ :: Int}
   | Dim4 {columnX, columnY, columnZ, columnC :: Int}


type Type = GraphType.T


columnToString :: Column -> String
columnToString col =
   case col of
      Dim3 x y z -> show x ++ ":" ++ show y ++ ":" ++ show z
      Dim4 x y z c -> show x ++ ":" ++ show y ++ ":" ++ show z ++ ":" ++ show c

toString :: T -> String
toString (Cons c t l) =
   "using " ++ columnToString c ++
   " with " ++ GraphType.toString t ++
   " " ++ LineSpec.toString l

instance Graph.C T where
   command _ = "splot"
   toString = toString


defaultType :: Type
defaultType = GraphType.pm3d

deflt :: Column -> T
deflt c = Cons c defaultType LineSpec.deflt


typ :: Type -> T -> T
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T -> T
lineSpec ls gr = gr{lineSpec_ = ls}
