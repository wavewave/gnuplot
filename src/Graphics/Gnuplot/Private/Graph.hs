module Graphics.Gnuplot.Private.Graph where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec

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


newtype Type = Type String


lines          :: Type
points         :: Type
linesPoints    :: Type
impulses       :: Type
dots           :: Type
steps          :: Type
fSteps         :: Type
hiSteps        :: Type
errorBars      :: Type
xErrorBars     :: Type
yErrorBars     :: Type
xyErrorBars    :: Type
errorLines     :: Type
xErrorLines    :: Type
yErrorLines    :: Type
xyErrorLines   :: Type
boxes          :: Type
filledCurves   :: Type
boxErrorBars   :: Type
boxXYErrorBars :: Type
financeBars    :: Type
candleSticks   :: Type
vectors        :: Type
pm3d           :: Type

lines          = Type "lines"
points         = Type "points"
linesPoints    = Type "linespoints"
impulses       = Type "impulses"
dots           = Type "dots"
steps          = Type "steps"
fSteps         = Type "fsteps"
hiSteps        = Type "histeps"
errorBars      = Type "errorbars"
xErrorBars     = Type "xerrorbars"
yErrorBars     = Type "yerrorbars"
xyErrorBars    = Type "xyerrorbars"
errorLines     = Type "errorlines"
xErrorLines    = Type "xerrorlines"
yErrorLines    = Type "yerrorlines"
xyErrorLines   = Type "xyerrorlines"
boxes          = Type "boxes"
filledCurves   = Type "filledcurves"
boxErrorBars   = Type "boxerrorbars"
boxXYErrorBars = Type "boxxyerrorbars"
financeBars    = Type "financebars"
candleSticks   = Type "candlesticks"
vectors        = Type "vectors"
pm3d           = Type "pm3d"


typeToString :: Type -> String
typeToString (Type t) = t


columnToString :: Column -> String
columnToString c =
   case c of
      Dim1 x -> show x
      Dim2 x y -> show x ++ ":" ++ show y

toString :: T -> String
toString (Cons c t l) =
   "using " ++ columnToString c ++
   " with " ++ typeToString t ++
   " " ++ LineSpec.toString l



defaultType :: Type
defaultType = lines

deflt :: Column -> T
deflt c = Cons c defaultType LineSpec.deflt


typ :: Type -> T -> T
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T -> T
lineSpec ls gr = gr{lineSpec_ = ls}
