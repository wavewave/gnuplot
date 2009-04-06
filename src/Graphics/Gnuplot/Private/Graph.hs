module Graphics.Gnuplot.Private.Graph where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec


data T =
   Cons {
      column_   :: Column,
      type_     :: Type,
      lineSpec_ :: LineSpec.T
   }

data Column =
     Dim1 {columnX :: Int}
   | Dim2 {columnX, columnY :: Int}


-- | in the future, Type should become the enumeration and PlotType should disappear
type Type = PlotType

data PlotType =
     Lines
   | Points
   | LinesPoints
   | Impulses
   | Dots
   | Steps
   | FSteps
   | HiSteps
   | ErrorBars
   | XErrorBars
   | YErrorBars
   | XYErrorBars
   | ErrorLines
   | XErrorLines
   | YErrorLines
   | XYErrorLines
   | Boxes
   | FilledCurves
   | BoxErrorBars
   | BoxXYErrorBars
   | FinanceBars
   | CandleSticks
   | Vectors
   | PM3d

typeToString :: Type -> String
typeToString t =
   case t of
      Lines          -> "lines"
      Points         -> "points"
      LinesPoints    -> "linespoints"
      Impulses       -> "impulses"
      Dots           -> "dots"
      Steps          -> "steps"
      FSteps         -> "fsteps"
      HiSteps        -> "histeps"
      ErrorBars      -> "errorbars"
      XErrorBars     -> "xerrorbars"
      YErrorBars     -> "yerrorbars"
      XYErrorBars    -> "xyerrorbars"
      ErrorLines     -> "errorlines"
      XErrorLines    -> "xerrorlines"
      YErrorLines    -> "yerrorlines"
      XYErrorLines   -> "xyerrorlines"
      Boxes          -> "boxes"
      FilledCurves   -> "filledcurves"
      BoxErrorBars   -> "boxerrorbars"
      BoxXYErrorBars -> "boxxyerrorbars"
      FinanceBars    -> "financebars"
      CandleSticks   -> "candlesticks"
      Vectors        -> "vectors"
      PM3d           -> "pm3d"


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
defaultType = Lines

deflt :: Column -> T
deflt c = Cons c defaultType LineSpec.deflt


typ :: Type -> T -> T
typ t gr = gr{type_ = t}

lineSpec :: LineSpec.T -> T -> T
lineSpec ls gr = gr{lineSpec_ = ls}
