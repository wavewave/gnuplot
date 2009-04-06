{-
The plot functions here return ExitCode,
which is nice for programming
but ugly for interactive GHCi sessions.
-}
module Graphics.Gnuplot.Advanced (
    Attribute(..),
    Size(..),
    Aspect(..),

--    Graph.Type(..),

    linearScale,

    terminal,

    plot2d,

{-
    Plot3dType(..),
    CornersToColor(..),
    Attribute3d(..),
    plotMesh3d,
    plotFunc3d,
-}

    epspdfPlot,
    inclPlot,
  ) where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Private.Plot  as Plot

import qualified Graphics.Gnuplot.Terminal.PostScript as PS
import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Execute as Exec


import System.Exit (ExitCode, )
import System.Cmd (rawSystem, )
import Graphics.Gnuplot.Utility
   (quote, commaConcat, showTriplet, )

import qualified Data.Monoid.State as State
import Data.Maybe (listToMaybe, mapMaybe, isNothing, )
import Data.List.HT (dropWhileRev, )



-- * User front-end

data Attribute =
     Custom String [String]  -- ^ anything that is allowed after gnuplot's @set@ command
   | Terminal Terminal.T     -- ^ you cannot use this, call 'terminal' instead
   | Grid   (Maybe [String])
   | Key    (Maybe [String])
   | Border (Maybe [String])
   | XTicks (Maybe [String])
   | YTicks (Maybe [String])
   | Size   (Size)
   | Aspect (Aspect)
   | BoxAspect (Aspect)
   | LineStyle Int LineSpec.T
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


-- candidate for Useful, similar routines are in module Integration
linearScale :: Fractional a => Integer -> (a,a) -> [a]
linearScale n (x0,x1) =
   map (\m -> x0 + (x1-x0) * fromIntegral m / fromIntegral n) [0..n]


terminal :: Terminal.C term => term -> Attribute
terminal =
   Terminal . Terminal.canonical


-- * plot functions

{-
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
   [Attribute] -> [Attribute3d] -> [[(a,b,c)]] -> IO ExitCode
plotMesh3d attrs pt dat =
   do writeFile tmpFile (unlines (map (unlines . map showTriplet) dat))
      Exec.simple
         (map attrToProg attrs ++
          ["set pm3d " ++ unwords (map attribute3dToString pt)] ++
          ["splot " ++ quote tmpFile ++ " using 1:2:3 with pm3"])
         ["-persist"]

{- |
> let xs = [-2,-1.8..2::Double] in plotFunc3d [] [] xs xs (\x y -> exp(-(x*x+y*y)))
-}
plotFunc3d :: (Show a, Show b, Show c) =>
   [Attribute] -> [Attribute3d] -> [b] -> [c] -> (b -> c -> a) -> IO ExitCode
plotFunc3d attrs pt xArgs yArgs f =
   plotMesh3d attrs pt (map (map (\(x,y) -> (x, y, f x y)) . flip map yArgs . (,)) xArgs)
-}


-- * For inclusion of gnuplot graphics in LaTeX documents using lhs2TeX

{-| Redirects the output of a plotting function to an EPS file
    and additionally converts it to PDF. -}
epspdfPlot ::
      FilePath
   -> ([Attribute] -> IO ExitCode)  {-^ Drawing function that expects some gnuplot attributes. -}
   -> IO ExitCode
epspdfPlot filename plot =
   do plot (terminal (PS.eps $ PS.cons (filename++".eps")) : Key Nothing : [])
      rawSystem "epstopdf" [filename++".eps"]

{-| Creates an EPS and a PDF graphics
    and returns a string that can be inserted into a LaTeX document
    to include this graphic.

    Different from GHCi, Hugs doesn't output a return value from an IO monad.
    So you must wrap it with a 'putStr'.
    Nevertheless this implementation which returns the LaTeX command as string
    is the most flexible one. -}
inclPlot ::
      FilePath
   -> ([Attribute] -> IO ExitCode)  {-^ Drawing function that expects some gnuplot attributes. -}
   -> IO String
inclPlot filename plot =
   do epspdfPlot filename plot
      return ("\\includegraphics{"++filename++"}")



-- * Internal functions


attrToProg :: Attribute -> String
attrToProg (Custom attribute parameters) =
   "set " ++ attribute ++ " " ++ unwords parameters

attrToProg (Terminal (Terminal.Cons options commands)) =
   "set terminal " ++ unwords options ++ ";" ++
   unwords commands

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
attrToProg (LineStyle num linespec) =
   "set linestyle " ++ show num ++ " " ++ LineSpec.toString linespec
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


{-
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
-}


plot2d :: [Attribute] -> Plot.T -> IO ExitCode
plot2d attrs (Plot.Cons mp) =
   let files = State.evaluate 0 mp
   in  do mapM_ Plot.writeData files
          callGnuplot attrs "plot" $
             concatMap (\(Plot.File filename _ grs) ->
                map (\gr -> quote filename ++ " " ++ Graph.toString gr) grs) $
             files

callGnuplot :: [Attribute] -> String -> [String] -> IO ExitCode
callGnuplot attrs cmd params =
   Exec.simple
      (map attrToProg attrs ++
       [cmd ++ " " ++
        extractRanges attrs ++ " " ++
        commaConcat params])
      ["-persist"]
   -- instead of the option, one can also use 'set terminal x11 persist'
