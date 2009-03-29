module Graphics.Gnuplot.Terminal.PostScript (
   T, cons,
   landscape, portrait, eps,
   color, monochrome,
   ) where

import qualified Graphics.Gnuplot.Terminal as Terminal
import Data.Maybe (catMaybes, )
import Graphics.Gnuplot.Utility (quote, )


data T =
   Cons {
      filename_ :: FilePath,
      mode_ :: Maybe Mode,
      color_ :: Maybe Bool
   }

cons :: FilePath -> T
cons path =
   Cons {
      filename_ = path,
      mode_ = Nothing,
      color_ = Nothing
   }


landscape :: T -> T
landscape = setMode Landscape

portrait :: T -> T
portrait = setMode Portrait

eps :: T -> T
eps = setMode EPS


color :: T -> T
color term =
   term{color_ = Just True}

monochrome :: T -> T
monochrome term =
   term{color_ = Just False}


-- private functions

data Mode =
     Landscape
   | Portrait
   | EPS

formatMode :: Mode -> String
formatMode mode =
   case mode of
      Landscape -> "landscape"
      Portrait  -> "portrait"
      EPS       -> "eps"

setMode :: Mode -> T -> T
setMode mode term = term{mode_ = Just mode}


instance Terminal.C T where
   canonical term =
      Terminal.Cons {
         Terminal.options =
            "postscript" :
            catMaybes (
               (fmap formatMode $ mode_ term) :
               (fmap (\b -> if b then "color" else "monochrome") $ color_ term) :
               []),
         Terminal.commands =
            "set" : "output" : (quote $ filename_ term) : []
      }
