module Graphics.Gnuplot.Terminal.SVG (
   T, cons,
   ) where

import qualified Graphics.Gnuplot.Private.Terminal as Terminal
import Graphics.Gnuplot.Utility (quote, )


data T =
   Cons {
      filename_ :: FilePath
   }

cons :: FilePath -> T
cons path =
   Cons {
      filename_ = path
   }



-- private functions

instance Terminal.C T where
   canonical term =
      Terminal.Cons {
         Terminal.options =
            "svg" :
            [],
         Terminal.commands =
            ["set output " ++ (quote $ filename_ term)],
         Terminal.interactive = False
      }
