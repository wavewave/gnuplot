module Graphics.Gnuplot.Simple (
    Attribute(..),
    Size(..),
    Aspect(..),

    LineSpec(..),
    PlotType(..),

    PlotStyle(..),

    linearScale,
    defaultStyle,

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

import System.Exit (ExitCode, )
import System.Cmd (rawSystem, )
import Control.Monad (zipWithM, )
import Data.Maybe (listToMaybe, mapMaybe, isNothing, )
import qualified Graphics.Gnuplot.Execute as Exec
import Graphics.Gnuplot.Utility
   (functionToGraph,
    quote, commaConcat, showTriplet, )
import Data.List.HT (dropWhileRev, )


{-* User front-end -}

data Attribute =
     Custom String [String]  -- ^ anything that is allowed after gnuplot's @set@ command
   | EPS    FilePath
   | PNG    FilePath
   | Grid   (Maybe [String])
   | Key    (Maybe [String])
   | Border (Maybe [String])
   | XTicks (Maybe [String])
   | YTicks (Maybe [String])
   | Size   (Size)
   | Aspect (Aspect)
   | BoxAspect (Aspect)
   | LineStyle Int LineSpec
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
   but their interpretations depend on the gnuplot output type. :-(
 -}

data LineSpec =
  LineSpec { lspecLineStyle :: Maybe Int
           , lspecLineType  :: Maybe Int
           , lspecLineWidth :: Maybe Double
           , lspecPointType :: Maybe Int
           , lspecPointSize :: Maybe Double
           , lspecTitle     :: Maybe String
           }

defaultLineSpec :: LineSpec
defaultLineSpec = LineSpec { lspecLineStyle = Nothing
                           , lspecLineType  = Nothing
                           , lspecLineWidth = Nothing
                           , lspecPointType = Nothing
                           , lspecPointSize = Nothing
                           , lspecTitle     = Nothing
                           }

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


-- candidate for Useful, similar routines are in module Integration
linearScale :: Fractional a => Integer -> (a,a) -> [a]
linearScale n (x0,x1) =
   map (\m -> x0 + (x1-x0) * fromIntegral m / fromIntegral n) [0..n]

defaultStyle :: PlotStyle
defaultStyle = PlotStyle { plotType = Lines
                         , lineSpec = defaultLineSpec
                         }

{- |
> plotList [] (take 30 (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs))
-}
plotList :: Show a => [Attribute] -> [a] -> IO ()
plotList attrs = plotListStyle attrs defaultStyle

plotListStyle :: Show a => [Attribute] -> PlotStyle -> [a] -> IO ()
plotListStyle attrs style dat =
   do writeFile tmpFile (unlines (map show dat))
      callGnuplot attrs "plot"
                  [quote tmpFile ++ " using 1 " ++
                   plotStyleToString style]
      return ()

plotLists :: Show a => [Attribute] -> [[a]] -> IO ()
plotLists attrs = plotListsStyle attrs . map ((,) defaultStyle)

plotListsStyle :: Show a => [Attribute] -> [(PlotStyle, [a])] -> IO ()
plotListsStyle attrs dats =
   do fileNames <- zipWithM
         (\n dat ->
             let fileName = tmpFileStem ++ show n ++ ".dat"
             in  writeFile fileName
                           (unlines (map show dat))
                    >> return fileName)
         [(1::Int)..] (map snd dats)
      callGnuplot attrs "plot"
         (zipWith
            (\fileName style ->
               quote fileName ++ " using 1 " ++
                  plotStyleToString style)
            fileNames (map fst dats))
      return ()

{- |
> plotFunc [] (linearScale 1000 (-10,10)) sin
-}
plotFunc :: Show a => [Attribute] -> [a] -> (a -> a) -> IO ()
plotFunc attrs args f = plotPath attrs (functionToGraph args f)

{- |
> plotFuncs [] (linearScale 1000 (-10,10)) [sin, cos]
-}
plotFuncs :: Show a => [Attribute] -> [a] -> [a -> a] -> IO ()
plotFuncs attrs args fs =
   plot2dMultiSharedAbscissa attrs (zipWith const (repeat defaultStyle) fs)
      (map (\x -> (x, map ($ x) fs)) args)

plotPath :: Show a => [Attribute] -> [(a,a)] -> IO ()
plotPath attrs = plot2dGen attrs defaultStyle

plotPaths :: Show a => [Attribute] -> [[(a,a)]] -> IO ()
plotPaths attrs = plot2dMultiGen attrs . zip (repeat defaultStyle)

plotPathStyle :: Show a => [Attribute] -> PlotStyle -> [(a,a)] -> IO ()
plotPathStyle = plot2dGen

plotPathsStyle :: Show a => [Attribute] -> [(PlotStyle, [(a,a)])] -> IO ()
plotPathsStyle = plot2dMultiGen

{- |
> plotParamFunc [] (linearScale 1000 (0,2*pi)) (\t -> (sin (2*t), cos t))
-}
plotParamFunc :: Show a => [Attribute] -> [a] -> (a -> (a,a)) -> IO ()
plotParamFunc attrs args f = plotPath attrs (map f args)

plotParamFuncs :: Show a => [Attribute] -> [a] -> [a -> (a,a)] -> IO ()
plotParamFuncs attrs args fs = plotPaths attrs (map (flip map args) fs)


plotDots :: Show a => [Attribute] -> [(a,a)] -> IO ()
plotDots attrs = plot2dGen attrs (defaultStyle { plotType = Dots })



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
   do writeFile tmpFile (unlines (map (unlines . map showTriplet) dat))
      Exec.simple
         (map attrToProg attrs ++
          ["set pm3d " ++ unwords (map attribute3dToString pt)] ++
          ["splot " ++ quote tmpFile ++ " using 1:2:3 with pm3"])
         ["-persist"]
      return ()

{- |
> let xs = [-2,-1.8..2::Double] in plotFunc3d [] [] xs xs (\x y -> exp(-(x*x+y*y)))
-}
plotFunc3d :: (Show a, Show b, Show c) =>
   [Attribute] -> [Attribute3d] -> [b] -> [c] -> (b -> c -> a) -> IO ()
plotFunc3d attrs pt xArgs yArgs f =
   plotMesh3d attrs pt (map (map (\(x,y) -> (x, y, f x y)) . flip map yArgs . (,)) xArgs)



{-* For inclusion of gnuplot graphics in LaTeX documents using lhs2TeX -}

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



{-* Internal functions -}

tmpFileStem, tmpFile :: FilePath

tmpFileStem = "curve"
tmpFile = tmpFileStem ++ ".dat"


attrToProg :: Attribute -> String
attrToProg (Custom attribute parameters) =
   "set " ++ attribute ++ " " ++ unwords parameters

attrToProg (EPS filename) =
   "set terminal postscript eps;" ++  -- latex
   "set output " ++ quote filename

attrToProg (PNG filename) =
   "set terminal png;" ++  -- latex
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
attrToProg (LineStyle num linespec)
   = "set linestyle " ++ show num ++ " " ++ (lineSpecToString linespec)
attrToProg (Title  title_)       = "set title " ++ quote title_
attrToProg (XLabel label)        = "set xlabel " ++ quote label
attrToProg (YLabel label)        = "set ylabel " ++ quote label
attrToProg (XRange _)            = ""  -- xrange is handled in plot command
attrToProg (YRange _)            = ""  -- yrange is handled in plot command
attrToProg (ZRange _)            = ""  -- zrange is handled in plot command
attrToProg (Palette colors)
   = "set palette defined (" ++
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
                    [xRangeFromAttr,  yRangeFromAttr, zRangeFromAttr]
       showRng (l,r) = "[" ++ show l ++ ":" ++ show r ++ "]"
   in  unwords (map (maybe "[:]" showRng) (dropWhileRev isNothing ranges))


lineSpecToString :: LineSpec -> String
lineSpecToString linespec =
  unwords $ [ s | Just s <- strings ]
  where
  strings = [ mMaybe "linetype"  show  $ lspecLineType  linespec
            , mMaybe "linewidth" show  $ lspecLineWidth linespec
            , mMaybe "pointtype" show  $ lspecPointType linespec
            , mMaybe "pointsize" show  $ lspecPointSize linespec
            , mMaybe "title"     quote $ lspecTitle     linespec
            , mMaybe "linestyle" show  $ lspecLineStyle linespec
            ]

  mMaybe :: String -> (a -> String) -> Maybe a -> Maybe String
  mMaybe _s _f Nothing   = Nothing
  mMaybe s  f  (Just a)  = Just $ unwords [ s, f a ]

plotTypeToString :: PlotType -> String
plotTypeToString Lines          = "lines"
plotTypeToString Points         = "points"
plotTypeToString LinesPoints    = "linespoints"
plotTypeToString Impulses       = "impulses"
plotTypeToString Dots           = "dots"
plotTypeToString Steps          = "steps"
plotTypeToString FSteps         = "fsteps"
plotTypeToString HiSteps        = "histeps"
plotTypeToString ErrorBars      = "errorbars"
plotTypeToString XErrorBars     = "xerrorbars"
plotTypeToString YErrorBars     = "yerrorbars"
plotTypeToString XYErrorBars    = "xyerrorbars"
plotTypeToString ErrorLines     = "errorlines"
plotTypeToString XErrorLines    = "xerrorlines"
plotTypeToString YErrorLines    = "yerrorlines"
plotTypeToString XYErrorLines   = "xyerrorlines"
plotTypeToString Boxes          = "boxes"
plotTypeToString FilledCurves   = "filledcurves"
plotTypeToString BoxErrorBars   = "boxerrorbars"
plotTypeToString BoxXYErrorBars = "boxxyerrorbars"
plotTypeToString FinanceBars    = "financebars"
plotTypeToString CandleSticks   = "candlesticks"
plotTypeToString Vectors        = "vectors"
plotTypeToString PM3d           = "pm3d"


plotStyleToString :: PlotStyle -> String
plotStyleToString (PlotStyle p l ) =
   "with " ++ plotTypeToString p ++ " " ++ lineSpecToString l


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


{-| Writes point data to a file and returns a string containing
    Gnuplot.plot parameters to invoke the file. -}
storeData :: Show a => FilePath -> PlotStyle -> [(a,a)] -> IO String
storeData file style dat =
   do writeFile file (unlines (map (\(x,y) -> show x ++ " " ++ show y) dat))
      return (quote file ++ " using 1:2 " ++ plotStyleToString style)

plot2dGen :: Show a => [Attribute] -> PlotStyle -> [(a,a)] -> IO ()
plot2dGen attrs style dat =
   do plotParam <- storeData tmpFile style dat
      callGnuplot attrs "plot" [plotParam]
      return ()

plot2dMultiGen :: Show a =>
   [Attribute] -> [(PlotStyle, [(a,a)])] -> IO ()
plot2dMultiGen attrs styleDat =
   do plotParams <- sequence $
         zipWith (\n -> uncurry (storeData (tmpFileStem++show n++".dat")))
                 [(0::Int)..] styleDat
      callGnuplot attrs "plot" plotParams
      return ()

plot2dMultiSharedAbscissa :: Show a =>
   [Attribute] -> [PlotStyle] -> [(a,[a])] -> IO ()
plot2dMultiSharedAbscissa attrs styles dat =
   let plotParams =
          zipWith (\n style -> quote tmpFile ++ " using 1:"
                       ++ show (n+1) ++ " " ++ plotStyleToString style)
                  [(1::Int)..] styles
   in  do {- writeFile tmpFile (concatMap (\(x,ys) ->
             foldr (\y -> shows y . (' ':)) (shows x "\n") ys) dat) -}
          writeFile tmpFile
             (unlines (map (unwords . map show . uncurry (:)) dat))
          callGnuplot attrs "plot" plotParams
          return ()

callGnuplot :: [Attribute] -> String -> [String] -> IO ExitCode
callGnuplot attrs cmd params =
   Exec.simple
      (map attrToProg attrs ++
       [cmd ++ " " ++
        extractRanges attrs ++ " " ++
        commaConcat params])
      ["-persist"]
   -- instead of the option, one can also use 'set terminal x11 persist'
