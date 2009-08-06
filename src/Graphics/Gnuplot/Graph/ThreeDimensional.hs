module Graphics.Gnuplot.Graph.ThreeDimensional (
   T,
   Type,

   defaultType,
   deflt,

   typ,
   lineSpec,

   lines,
   points,
   linesPoints,
   impulses,
   dots,
   steps,
   fSteps,
   hiSteps,
   errorBars,
   xErrorBars,
   yErrorBars,
   xyErrorBars,
   errorLines,
   xErrorLines,
   yErrorLines,
   xyErrorLines,
   boxes,
   filledCurves,
   boxErrorBars,
   boxXYErrorBars,
   financeBars,
   candleSticks,
   vectors,
   pm3d,
   ) where

import Graphics.Gnuplot.Private.Graph3D
import Graphics.Gnuplot.Private.GraphType hiding (T, )

import Prelude hiding (lines, )
