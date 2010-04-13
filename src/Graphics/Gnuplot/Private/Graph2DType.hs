module Graphics.Gnuplot.Private.Graph2DType where

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import Prelude hiding (lines, )


newtype T a = Cons String

tupleSize :: (Tuple.C a) => T a -> Int
tupleSize =
   let size :: (Tuple.C a) => T a -> (Int, a)
       size _ = Tuple.number
   in  fst . size


{-
See info:/gnuplot/set_style_rectangle
-}

listLines       :: (Atom.C x) => T x
listPoints      :: (Atom.C x) => T x
listLinesPoints :: (Atom.C x) => T x
listImpulses    :: (Atom.C x) => T x
listDots        :: (Atom.C x) => T x

xErrorBarsRelative   :: (Atom.C x, Atom.C y) => T ((x,y),x)
yErrorBarsRelative   :: (Atom.C x, Atom.C y) => T ((x,y),y)
xyErrorBarsRelative  :: (Atom.C x, Atom.C y) => T ((x,y),(x,y))
xErrorBarsAbsolute   :: (Atom.C x, Atom.C y) => T ((x,y),(x,x))
yErrorBarsAbsolute   :: (Atom.C x, Atom.C y) => T ((x,y),(y,y))
xyErrorBarsAbsolute  :: (Atom.C x, Atom.C y) => T ((x,y),((x,x),(y,y)))

xErrorLinesRelative  :: (Atom.C x, Atom.C y) => T ((x,y),x)
yErrorLinesRelative  :: (Atom.C x, Atom.C y) => T ((x,y),y)
xyErrorLinesRelative :: (Atom.C x, Atom.C y) => T ((x,y),(x,y))
xErrorLinesAbsolute  :: (Atom.C x, Atom.C y) => T ((x,y),(x,x))
yErrorLinesAbsolute  :: (Atom.C x, Atom.C y) => T ((x,y),(y,y))
xyErrorLinesAbsolute :: (Atom.C x, Atom.C y) => T ((x,y),((x,x),(y,y)))

lines          :: (Atom.C x, Atom.C y) => T (x,y)
points         :: (Atom.C x, Atom.C y) => T (x,y)
linesPoints    :: (Atom.C x, Atom.C y) => T (x,y)
impulses       :: (Atom.C x, Atom.C y) => T (x,y)
dots           :: (Atom.C x, Atom.C y) => T (x,y)
steps          :: (Atom.C x, Atom.C y) => T (x,y)
fSteps         :: (Atom.C x, Atom.C y) => T (x,y)
hiSteps        :: (Atom.C x, Atom.C y) => T (x,y)
errorBars      :: (Atom.C x, Atom.C y) => T (x,y)
errorLines     :: (Atom.C x, Atom.C y) => T (x,y)
boxes          :: (Atom.C x, Atom.C y) => T (x,y)
filledCurves   :: (Atom.C x, Atom.C y) => T (x,y)
{-
boxErrorBars   :: (Atom.C x, Atom.C y) => T (x,y)
boxXYErrorBars :: (Atom.C x, Atom.C y) => T (x,y)
-}
financeBars    :: (Atom.C x, Atom.C y) => T (x,(y,y,y,y))
candleSticks   :: (Atom.C x, Atom.C y) => T (x,(y,y,y,y))
vectors        :: (Atom.C x, Atom.C y) => T ((x,y),(x,y))


listLines       = Cons "lines"
listPoints      = Cons "points"
listLinesPoints = Cons "linespoints"
listImpulses    = Cons "impulses"
listDots        = Cons "dots"

xErrorBarsRelative   = Cons "xerrorbars"
yErrorBarsRelative   = Cons "yerrorbars"
xyErrorBarsRelative  = Cons "xyerrorbars"
xErrorBarsAbsolute   = Cons "xerrorbars"
yErrorBarsAbsolute   = Cons "yerrorbars"
xyErrorBarsAbsolute  = Cons "xyerrorbars"

xErrorLinesRelative  = Cons "xerrorlines"
yErrorLinesRelative  = Cons "yerrorlines"
xyErrorLinesRelative = Cons "xyerrorlines"
xErrorLinesAbsolute  = Cons "xerrorlines"
yErrorLinesAbsolute  = Cons "yerrorlines"
xyErrorLinesAbsolute = Cons "xyerrorlines"

lines          = Cons "lines"
points         = Cons "points"
linesPoints    = Cons "linespoints"
impulses       = Cons "impulses"
dots           = Cons "dots"
steps          = Cons "steps"
fSteps         = Cons "fsteps"
hiSteps        = Cons "histeps"
errorBars      = Cons "errorbars"
errorLines     = Cons "errorlines"
boxes          = Cons "boxes"
filledCurves   = Cons "filledcurves"
{-
boxErrorBars   = Cons "boxerrorbars"
boxXYErrorBars = Cons "boxxyerrorbars"
-}
financeBars    = Cons "financebars"
candleSticks   = Cons "candlesticks"
vectors        = Cons "vectors"


toString :: T a -> String
toString (Cons t) = t
