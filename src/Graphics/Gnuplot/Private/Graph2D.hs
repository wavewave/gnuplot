module Graphics.Gnuplot.Private.Graph2D where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.GraphType as GraphType

import Prelude hiding (lines, )


data T =
   Cons {
      column_   :: Column,
      type_     :: Type,
      lineSpec_ :: LineSpec.T
   }

data Column =
     Dim1 {columnX :: Int}
   | Dim2 {columnX, columnY :: Int}


type Type = GraphType.T


columnToString :: Column -> String
columnToString c =
   case c of
      Dim1 x -> show x
      Dim2 x y -> show x ++ ":" ++ show y

toString :: T -> String
toString (Cons c t l) =
   "using " ++ columnToString c ++
   " with " ++ GraphType.toString t ++
   " " ++ LineSpec.toString l



defaultType :: Type
defaultType = GraphType.lines

deflt :: Column -> T
deflt c = Cons c defaultType LineSpec.deflt


typ :: Type -> T -> T
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T -> T
lineSpec ls gr = gr{lineSpec_ = ls}
