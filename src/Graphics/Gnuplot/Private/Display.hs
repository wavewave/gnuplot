module Graphics.Gnuplot.Private.Display where

import qualified Graphics.Gnuplot.Private.FrameOption as Option
import qualified Graphics.Gnuplot.Private.File as File

import qualified Data.Map as Map
import qualified Data.Monoid.State as State
import Data.Monoid (Monoid, mempty, mappend, )


newtype Script =
   Script {
      runScript :: State.T (Int, OptionSet) Body
   }

pure :: Body -> Script
pure = Script . State.pure

data Body =
   Body {
      files :: [File],
      commands :: [String]
   }

type OptionSet = Map.Map Option.T [String]

data File =
   File {
      filename :: FilePath,
      content :: String
   }

instance File.C File where
   write (File fn cont) =
      writeFile fn cont


{-
Could also be implemented with Control.Monad.Trans.State:
mappend = liftA2 mappend
-}
instance Monoid Script where
   mempty = Script mempty
   mappend (Script b0) (Script b1) =
      Script (mappend b0 b1)

{-
Could also be implemented with Control.Monad.Trans.State:
mappend = liftA2 mappend
-}
instance Monoid Body where
   mempty = Body mempty mempty
   mappend (Body f0 c0) (Body f1 c1) =
      Body (mappend f0 f1) (mappend c0 c1)

class C gfx where
   toScript :: gfx -> Script
