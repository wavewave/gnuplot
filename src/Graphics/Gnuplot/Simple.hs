{- |
This is a simple monolithic interface to gnuplot
that can be used as is in GHCi or Hugs.
We do not plan to support every feature of gnuplot here,
instead we provide an advanced modularized interface
in "Graphics.Gnuplot.Advanced".
-}
module Graphics.Gnuplot.Simple (
    Attribute(..),
    Size(..),
    Aspect(..),

    LineAttr(..),
    LineSpec(..),
    PlotType(..),

    PlotStyle(..),

    linearScale,
    defaultStyle,

    terminal,

    plotList,
    plotListStyle,
    plotLists,
    plotListsStyle,
    plotFunc,
    plotFuncs,
    plotPath,
    plotPaths,
    plotPathStyle,
    plotPathsStyle,
    plotParamFunc,
    plotParamFuncs,
    plotDots,

    Plot3dType(..),
    CornersToColor(..),
    Attribute3d(..),
    plotMesh3d,
    plotFunc3d,

    epspdfPlot,
    inclPlot,
  ) where

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph2D as Graph2D
import qualified Graphics.Gnuplot.Private.Graph3D as Graph3D
import qualified Graphics.Gnuplot.Private.GraphType as GraphType
import qualified Graphics.Gnuplot.Private.Plot as Plot

{-
import qualified Graphics.Gnuplot.Terminal.PostScript as PS
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
-}

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Execute as Exec

import System.Cmd (rawSystem, )
import Graphics.Gnuplot.Utility
   (quote, commaConcat, showTriplet, linearScale, )
import qualified Data.Monoid.State as State
import Data.Maybe (listToMaybe, mapMaybe, isNothing, )
import Data.List.HT (dropWhileRev, )
import Data.List (intersperse, )
import Data.Monoid (mconcat, )


-- * User front-end

data Attribute =
     Custom String [String]  -- ^ anything that is allowed after gnuplot's @set@ command
   | EPS    FilePath
   | PNG    FilePath
   | Terminal Terminal.T     -- ^ you cannot use this, call 'terminal' instead
   | Grid   (Maybe [String])
   | Key    (Maybe [String])
   | Border (Maybe [String])
   | XTicks (Maybe [String])
   | YTicks (Maybe [String])
   | Size   (Size)
   | Aspect (Aspect)
   | BoxAspect (Aspect)
   | LineStyle Int [LineAttr]
   | Title  String
   | XLabel String
   | YLabel String
   | XRange (Double, Double)
   | YRange (Double, Double)
   | ZRange (Double, Double)
   | Palette [(Double, (Double, Double, Double))]
   | ColorBox (Maybe [String])
   | XTime
   | XFormat String

data Size =
     Scale    Double
   | SepScale Double Double

data Aspect =
     Ratio Double
   | NoRatio

{- The Int types would be better enumerations
   but their interpretations depend on the gnuplot output type. :-( -}
data LineAttr =
     LineType  Int
   | LineWidth Double
   | PointType Int
   | PointSize Double
   | LineTitle String

data LineSpec =
     DefaultStyle Int
   | CustomStyle  [LineAttr]

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

data PlotStyle = PlotStyle { plotType :: PlotType, lineSpec :: LineSpec }



defaultStyle :: PlotStyle
defaultStyle = PlotStyle Lines (CustomStyle [])


terminal :: Terminal.C term => term -> Attribute
terminal =
   Terminal . Terminal.canonical


-- * plot functions

{- |
> plotList [] (take 30 (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs))
-}
plotList :: Show a => [Attribute] -> [a] -> IO ()
plotList attrs =
   plot2d attrs . Plot2D.list

plotListStyle :: Show a => [Attribute] -> PlotStyle -> [a] -> IO ()
plotListStyle attrs style =
   plot2d attrs . setPlotStyle style . Plot2D.list

plotLists :: Show a => [Attribute] -> [[a]] -> IO ()
plotLists attrs xss =
   plot2d attrs (mconcat $ map Plot2D.list xss)

plotListsStyle :: Show a => [Attribute] -> [(PlotStyle, [a])] -> IO ()
plotListsStyle attrs =
   plot2d attrs . mconcat .
   map (\(style,xs) -> setPlotStyle style $ Plot2D.list xs)

{- |
> plotFunc [] (linearScale 1000 (-10,10)) sin
-}
plotFunc :: Show a => [Attribute] -> [a] -> (a -> a) -> IO ()
plotFunc attrs args f =
   plot2d attrs (Plot2D.function args f)

{- |
> plotFuncs [] (linearScale 1000 (-10,10)) [sin, cos]
-}
plotFuncs :: Show a => [Attribute] -> [a] -> [a -> a] -> IO ()
plotFuncs attrs args fs =
   plot2d attrs (Plot2D.functions args fs)

plotPath :: Show a => [Attribute] -> [(a,a)] -> IO ()
plotPath attrs =
   plot2d attrs . Plot2D.path

plotPaths :: Show a => [Attribute] -> [[(a,a)]] -> IO ()
plotPaths attrs xss =
   plot2d attrs (mconcat $ map Plot2D.path xss)

plotPathStyle :: Show a => [Attribute] -> PlotStyle -> [(a,a)] -> IO ()
plotPathStyle attrs style =
   plot2d attrs . setPlotStyle style . Plot2D.path

plotPathsStyle :: Show a => [Attribute] -> [(PlotStyle, [(a,a)])] -> IO ()
plotPathsStyle attrs =
   plot2d attrs . mconcat .
   map (\(style,xs) -> setPlotStyle style $ Plot2D.path xs)

{- |
> plotParamFunc [] (linearScale 1000 (0,2*pi)) (\t -> (sin (2*t), cos t))
-}
plotParamFunc :: Show a => [Attribute] -> [a] -> (a -> (a,a)) -> IO ()
plotParamFunc attrs args f =
   plot2d attrs (Plot2D.parameterFunction args f)

{- |
> plotParamFuncs [] (linearScale 1000 (0,2*pi)) [\t -> (sin (2*t), cos t), \t -> (cos t, sin (2*t))]
-}
plotParamFuncs :: Show a => [Attribute] -> [a] -> [a -> (a,a)] -> IO ()
plotParamFuncs attrs args fs =
   plot2d attrs (mconcat $ map (Plot2D.parameterFunction args) fs)


plotDots :: Show a => [Attribute] -> [(a,a)] -> IO ()
plotDots attrs xs =
   plot2d attrs (fmap (Graph2D.typ GraphType.dots) $ Plot2D.path xs)



data Plot3dType =
     Surface
   | ColorMap

data CornersToColor =
     Mean
   | GeometricMean
   | Median
   | Corner1
   | Corner2
   | Corner3
   | Corner4

data Attribute3d =
     Plot3dType     Plot3dType
   | CornersToColor CornersToColor


{- |
> let xs = [-2,-1.8..2::Double] in plotMesh3d [] [] (do x <- xs; return (do y <- xs; return (x,y,cos(x*x+y*y))))
-}
plotMesh3d :: (Show a, Show b, Show c) =>
   [Attribute] -> [Attribute3d] -> [[(a,b,c)]] -> IO ()
plotMesh3d attrs pt dat =
   plot3d attrs pt (Plot3D.mesh dat)

{- |
> let xs = [-2,-1.8..2::Double] in plotFunc3d [] [] xs xs (\x y -> exp(-(x*x+y*y)))
-}
plotFunc3d :: (Show a, Show b, Show c) =>
   [Attribute] -> [Attribute3d] -> [b] -> [c] -> (b -> c -> a) -> IO ()
plotFunc3d attrs pt xArgs yArgs f =
   plot3d attrs pt (Plot3D.function xArgs yArgs f)



-- * For inclusion of gnuplot graphics in LaTeX documents using lhs2TeX

{-| Redirects the output of a plotting function to an EPS file
    and additionally converts it to PDF. -}
epspdfPlot ::
      FilePath
   -> ([Attribute] -> IO ())  {-^ Drawing function that expects some gnuplot attributes. -}
   -> IO ()
epspdfPlot filename plot =
   do plot (EPS (filename++".eps") : Key Nothing : [])
      rawSystem "epstopdf" [filename++".eps"]
      return ()

{-| Creates an EPS and a PDF graphics
    and returns a string that can be inserted into a LaTeX document
    to include this graphic.

    Different from GHCi, Hugs doesn't output a return value from an IO monad.
    So you must wrap it with a 'putStr'.
    Nevertheless this implementation which returns the LaTeX command as string
    is the most flexible one. -}
inclPlot ::
      FilePath
   -> ([Attribute] -> IO ())  {-^ Drawing function that expects some gnuplot attributes. -}
   -> IO String
inclPlot filename plot =
   do epspdfPlot filename plot
      return ("\\includegraphics{"++filename++"}")



-- * Internal functions

attrToProg :: Attribute -> String
attrToProg (Custom attribute parameters) =
   "set " ++ attribute ++ " " ++ unwords parameters

attrToProg (Terminal (Terminal.Cons options commands)) =
   concat $
   intersperse "; " $
   ("set terminal " ++ unwords options) : commands

attrToProg (EPS filename) =
   "set terminal postscript eps; " ++  -- latex
   "set output " ++ quote filename

attrToProg (PNG filename) =
   "set terminal png; " ++
   "set output " ++ quote filename

attrToProg (Grid   (Just x))     = "set grid " ++ unwords x
attrToProg (Grid   Nothing)      = "set nogrid"
attrToProg (Key    (Just x))     = "set key " ++ unwords x
attrToProg (Key    Nothing)      = "set nokey"
attrToProg (Border (Just x))     = "set border " ++ unwords x
attrToProg (Border Nothing)      = "set noborder"
attrToProg (XTicks (Just x))     = "set xtics " ++ unwords x
attrToProg (XTicks Nothing)      = "set noxtics"
attrToProg (YTicks (Just x))     = "set ytics " ++ unwords x
attrToProg (YTicks Nothing)      = "set noytics"
attrToProg (Size (Scale c))      = "set size " ++ show c
attrToProg (Size (SepScale x y)) = "set size " ++ show x ++ ", " ++ show y
attrToProg (Aspect (Ratio r))    = "set size ratio " ++ show (-r)
attrToProg (Aspect (NoRatio))    = "set size noratio"
attrToProg (BoxAspect (Ratio r)) = "set size ratio " ++ show r
attrToProg (BoxAspect (NoRatio)) = "set size noratio"
attrToProg (LineStyle num style) =
   "set linestyle " ++ show num ++ " " ++
   LineSpec.toString (lineAttrRecord style LineSpec.deflt)
attrToProg (Title  title_)       = "set title " ++ quote title_
attrToProg (XLabel label)        = "set xlabel " ++ quote label
attrToProg (YLabel label)        = "set ylabel " ++ quote label
attrToProg (XRange _)            = ""  -- xrange is handled in plot command
attrToProg (YRange _)            = ""  -- yrange is handled in plot command
attrToProg (ZRange _)            = ""  -- zrange is handled in plot command
attrToProg (Palette colors) =
   "set palette defined (" ++
     commaConcat (map (\(idx,c) -> show idx ++ " " ++ showTriplet c) colors) ++ ")"
attrToProg (ColorBox (Just x))     = "set colorbox " ++ unwords x
attrToProg (ColorBox Nothing)      = "unset colorbox"
attrToProg XTime                   = "set xdata time; set timefmt \"%s\""
attrToProg (XFormat fmt)           = "set format x " ++ quote fmt

xRangeFromAttr, yRangeFromAttr, zRangeFromAttr ::
   Attribute -> Maybe (Double, Double)
xRangeFromAttr (XRange rng) = Just rng
xRangeFromAttr _            = Nothing

yRangeFromAttr (YRange rng) = Just rng
yRangeFromAttr _            = Nothing

zRangeFromAttr (ZRange rng) = Just rng
zRangeFromAttr _            = Nothing

extractRanges :: [Attribute] -> String
extractRanges attrs =
   let ranges = map (listToMaybe . flip mapMaybe attrs)
                    [xRangeFromAttr, yRangeFromAttr, zRangeFromAttr]
       showRng (l,r) = "[" ++ show l ++ ":" ++ show r ++ "]"
   in  unwords (map (maybe "[:]" showRng) (dropWhileRev isNothing ranges))



plotTypeToGraph :: PlotType -> GraphType.T
plotTypeToGraph t =
   case t of
      Lines          -> GraphType.lines
      Points         -> GraphType.points
      LinesPoints    -> GraphType.linesPoints
      Impulses       -> GraphType.impulses
      Dots           -> GraphType.dots
      Steps          -> GraphType.steps
      FSteps         -> GraphType.fSteps
      HiSteps        -> GraphType.hiSteps
      ErrorBars      -> GraphType.errorBars
      XErrorBars     -> GraphType.xErrorBars
      YErrorBars     -> GraphType.yErrorBars
      XYErrorBars    -> GraphType.xyErrorBars
      ErrorLines     -> GraphType.errorLines
      XErrorLines    -> GraphType.xErrorLines
      YErrorLines    -> GraphType.yErrorLines
      XYErrorLines   -> GraphType.xyErrorLines
      Boxes          -> GraphType.boxes
      FilledCurves   -> GraphType.filledCurves
      BoxErrorBars   -> GraphType.boxErrorBars
      BoxXYErrorBars -> GraphType.boxXYErrorBars
      FinanceBars    -> GraphType.financeBars
      CandleSticks   -> GraphType.candleSticks
      Vectors        -> GraphType.vectors
      PM3d           -> GraphType.pm3d


plot3dTypeToString :: Plot3dType -> String
plot3dTypeToString Surface  = ""
plot3dTypeToString ColorMap = "map"

cornersToColorToString :: CornersToColor -> String
cornersToColorToString Mean          = "mean"
cornersToColorToString GeometricMean = "geomean"
cornersToColorToString Median        = "median"
cornersToColorToString Corner1       = "c1"
cornersToColorToString Corner2       = "c2"
cornersToColorToString Corner3       = "c3"
cornersToColorToString Corner4       = "c4"

attribute3dToString :: Attribute3d -> String
attribute3dToString (Plot3dType     pt)  = plot3dTypeToString pt
attribute3dToString (CornersToColor c2c) =
   "corners2color " ++cornersToColorToString c2c



plot2d :: [Attribute] -> Plot2D.T -> IO ()
plot2d attrs (Plot.Cons mp) =
   let files = State.evaluate 0 mp
   in  do mapM_ Plot.writeData files
          callGnuplot attrs "plot" $
             concatMap (\(Plot.File filename _ grs) ->
                map (\gr -> quote filename ++ " " ++ Graph2D.toString gr) grs) $
             files

setPlotStyle :: PlotStyle -> Plot2D.T -> Plot2D.T
setPlotStyle ps =
   fmap (Graph2D.typ (plotTypeToGraph $ plotType ps) .
         Graph2D.lineSpec (lineSpecRecord $ lineSpec ps))


plot3d :: [Attribute] -> [Attribute3d] -> Plot3D.T -> IO ()
plot3d attrs pt (Plot.Cons mp) =
   let files = State.evaluate 0 mp
   in  do mapM_ Plot.writeData files
          callGnuplot
             (attrs ++ [Custom "pm3d" (map attribute3dToString pt)]) "splot" $
             concatMap (\(Plot.File filename _ grs) ->
                map (\gr -> quote filename ++ " " ++ Graph3D.toString gr) grs) $
             files


lineSpecRecord :: LineSpec -> LineSpec.T
lineSpecRecord (DefaultStyle n) =
   LineSpec.lineStyle n LineSpec.deflt
lineSpecRecord (CustomStyle ls) =
   lineAttrRecord ls LineSpec.deflt

lineAttrRecord :: [LineAttr] -> LineSpec.T -> LineSpec.T
lineAttrRecord =
   flip $ foldl (flip $ \attr ->
      case attr of
         LineType  n -> LineSpec.lineType  n
         LineWidth w -> LineSpec.lineWidth w
         PointType n -> LineSpec.pointType n
         PointSize s -> LineSpec.pointSize s
         LineTitle s -> LineSpec.title     s
      )

callGnuplot :: [Attribute] -> String -> [String] -> IO ()
callGnuplot attrs cmd params =
   Exec.simple
      (map attrToProg attrs ++
       [cmd ++ " " ++
        extractRanges attrs ++ " " ++
        commaConcat params])
      ["-persist"]
   -- instead of the option, one can also use 'set terminal x11 persist'
     >> return ()
