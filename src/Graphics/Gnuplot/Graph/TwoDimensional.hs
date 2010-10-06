module Graphics.Gnuplot.Graph.TwoDimensional (
   T,
   Type,

--   deflt,

   lineSpec,

   listLines,
   listPoints,
   listLinesPoints,
   listImpulses,
   listDots,

   xErrorBarsRelative,
   yErrorBarsRelative,
   xyErrorBarsRelative,
   xErrorBarsAbsolute,
   yErrorBarsAbsolute,
   xyErrorBarsAbsolute,

   xErrorLinesRelative,
   yErrorLinesRelative,
   xyErrorLinesRelative,
   xErrorLinesAbsolute,
   yErrorLinesAbsolute,
   xyErrorLinesAbsolute,

   Graph2DType.lines,
   points,
   linesPoints,
   impulses,
   dots,
   steps,
   fSteps,
   hiSteps,
   errorBars,
   errorLines,
   boxes,
   filledCurves,
{-
   boxErrorBars,
   boxXYErrorBars,
-}
   financeBars,
   candleSticks,
   vectors,
   ) where

import Graphics.Gnuplot.Private.Graph2D
import Graphics.Gnuplot.Private.Graph2DType as Graph2DType hiding (T, )
