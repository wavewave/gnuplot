module Graphics.Gnuplot.Private.GraphType where

import Prelude hiding (lines, )


newtype T = Cons String


lines          :: T
points         :: T
linesPoints    :: T
impulses       :: T
dots           :: T
steps          :: T
fSteps         :: T
hiSteps        :: T
errorBars      :: T
xErrorBars     :: T
yErrorBars     :: T
xyErrorBars    :: T
errorLines     :: T
xErrorLines    :: T
yErrorLines    :: T
xyErrorLines   :: T
boxes          :: T
filledCurves   :: T
boxErrorBars   :: T
boxXYErrorBars :: T
financeBars    :: T
candleSticks   :: T
vectors        :: T
pm3d           :: T

lines          = Cons "lines"
points         = Cons "points"
linesPoints    = Cons "linespoints"
impulses       = Cons "impulses"
dots           = Cons "dots"
steps          = Cons "steps"
fSteps         = Cons "fsteps"
hiSteps        = Cons "histeps"
errorBars      = Cons "errorbars"
xErrorBars     = Cons "xerrorbars"
yErrorBars     = Cons "yerrorbars"
xyErrorBars    = Cons "xyerrorbars"
errorLines     = Cons "errorlines"
xErrorLines    = Cons "xerrorlines"
yErrorLines    = Cons "yerrorlines"
xyErrorLines   = Cons "xyerrorlines"
boxes          = Cons "boxes"
filledCurves   = Cons "filledcurves"
boxErrorBars   = Cons "boxerrorbars"
boxXYErrorBars = Cons "boxxyerrorbars"
financeBars    = Cons "financebars"
candleSticks   = Cons "candlesticks"
vectors        = Cons "vectors"
pm3d           = Cons "pm3d"


toString :: T -> String
toString (Cons t) = t
