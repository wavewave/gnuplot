module Graphics.Gnuplot.Private.Graph2D where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.FrameOption as Option
import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph2DType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Data.Map as Map
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


type AxisOption x y a =
   OptionSet.T (T x y) -> (Atom.OptionSet, a)

defltOptions :: (Atom.C x, Atom.C y) => OptionSet.T (T x y)
defltOptions =
   let optX :: Atom.C x => AxisOption x y x
       optX _ = Atom.options
       optY :: Atom.C y => AxisOption x y y
       optY _ = Atom.options
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
          (Option.zData, []) :
          (Option.zFormat, []) :
          []
   in  result


instance (Atom.C x, Atom.C y) => Graph.C (T x y) where
   command _ = "plot"
   toString = toString
   defltOptions = defltOptions


deflt :: GraphType.T x y a -> Column -> T x y
deflt t c = Cons c (GraphType.toString t) LineSpec.deflt

typ :: Type -> T x y -> T x y
typ t gr = gr{type_ = t}

{- |
You can alter the line specification of graphs in a plot using 'fmap'.
-}
lineSpec :: LineSpec.T -> T x y -> T x y
lineSpec ls gr = gr{lineSpec_ = ls}
