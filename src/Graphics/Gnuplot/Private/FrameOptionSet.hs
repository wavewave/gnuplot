{-
This controls gnuplot variables for a plot.
We use a separate data type for that purpose
in order to be able to share a set of options between different plots.
(If we would only allow to set options of a frame,
e.g. border :: Bool -> Frame -> Frame,
we could of course still build
an options setting function of type Frame -> Frame
from smaller option setters.)
-}
module Graphics.Gnuplot.Private.FrameOptionSet where

import qualified Graphics.Gnuplot.Private.FrameOption as Option
-- import Graphics.Gnuplot.Utility (quote, )

import qualified Data.Map as Map

import Data.Maybe.HT (toMaybe, )
import Data.Maybe (mapMaybe, )


type Plain = Map.Map Option.T [String]

newtype T graph =
   Cons {decons :: Plain}


{- |
The default options contain what we expect as default value in gnuplot.
We need an entry for every option that cannot be reset by @unset@.
-}
deflt :: Plain
deflt =
   Map.fromList $
   (Option.keyShow, []) :
--   (Option.border, []) :
   (Option.xLabelText, []) :
   (Option.yLabelText, []) :
   (Option.zLabelText, []) :
   (Option.xRangeBounds, ["[*:*]"]) :
   (Option.yRangeBounds, ["[*:*]"]) :
   (Option.zRangeBounds, ["[*:*]"]) :
   (Option.xFormat, []) :
   (Option.yFormat, []) :
   (Option.zFormat, []) :
   (Option.xTickLabels, []) :
   (Option.yTickLabels, []) :
   (Option.zTickLabels, []) :
--   (Option.timeFmt, [quote "%s"]) :
   []

initial :: Plain
initial =
   flip Map.union deflt $
   Map.fromList $
   (Option.xData, []) :
   (Option.yData, []) :
   (Option.zData, []) :
--   (Option.timeFmt, []) :
   []

{- |
Add an option with arguments as plain strings.
This is very flexible, but not very safe.
Use it only as fall-back,
if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".
-}
add :: Option.T -> [String] -> T graph -> T graph
add opt args (Cons m) =
   Cons (Map.insert opt args m)

{- |
Remove an option.
-}
remove :: Option.T -> T graph -> T graph
remove opt (Cons m) =
   Cons (Map.delete opt m)

{- |
Convert the difference between the first and the second option set
into a sequence of 'set' and 'unset' commands.
-}
diffToString :: Plain -> Plain -> [String]
diffToString m0 m1 =
   mapMaybe
      (\(Option.Cons opt _, (old,new)) ->
         toMaybe (old/=new) $
         maybe
            ("unset " ++ opt)
            (\args -> "set " ++ opt ++ concatMap (' ':) args)
            new) $
   Map.toList $
   Map.unionWith
      (\(old,_) (_,new) -> (old,new))
      (fmap (\x -> (Just x, Nothing)) m0)
      (fmap (\x -> (Nothing, Just x)) m1)
