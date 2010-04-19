module Graphics.Gnuplot.Private.Graph3D where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.FrameOption as Option
import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph3DType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Data.Map as Map

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


type AxisOption x y z a =
   OptionSet.T (T x y z) -> (Atom.OptionSet, a)

defltOptions :: (Atom.C x, Atom.C y, Atom.C z) => OptionSet.T (T x y z)
defltOptions =
   let optX :: Atom.C x => AxisOption x y z x
       optX _ = Atom.options
       optY :: Atom.C y => AxisOption x y z y
       optY _ = Atom.options
       optZ :: Atom.C z => AxisOption x y z z
       optZ _ = Atom.options
       mk :: Option.T -> Option.T ->
             (Atom.OptionSet, a) -> [(Option.T, [String])]
       mk optData optFormat op =
          let opts = fst op
          in  (optData, Atom.optData opts) :
              (optFormat, Atom.optFormat opts) :
              Atom.optOthers opts
       result =
          OptionSet.Cons $
          flip Map.union OptionSet.deflt $
          Map.fromList $
          mk Option.xData Option.xFormat (optX result) ++
          mk Option.yData Option.yFormat (optY result) ++
          mk Option.yData Option.yFormat (optZ result) ++
          []
   in  result


instance (Atom.C x, Atom.C y, Atom.C z) => Graph.C (T x y z) where
   command _ = "splot"
   toString = toString
   defltOptions = defltOptions


deflt :: GraphType.T x y z a -> Column -> T x y z
deflt t c = Cons c (GraphType.toString t) LineSpec.deflt

typ :: Type -> T x y z -> T x y z
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T x y z -> T x y z
lineSpec ls gr = gr{lineSpec_ = ls}
