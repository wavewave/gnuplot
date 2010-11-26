module Graphics.Gnuplot.Terminal.PostScript (
   T, cons,
   landscape, portrait, eps,
   color, monochrome,
   font, embedFont,
   ) where

import qualified Graphics.Gnuplot.Terminal as Terminal
import Graphics.Gnuplot.Utility (quote, )
import Data.Maybe (maybeToList, )


data T =
   Cons {
      filename_ :: FilePath,
      mode_ :: Maybe Mode,
      color_ :: Maybe Bool,
      embedFont_ :: [FilePath],
      font_ :: Maybe (String, Int)
   }

cons :: FilePath -> T
cons path =
   Cons {
      filename_ = path,
      mode_ = Nothing,
      color_ = Nothing,
      embedFont_ = [],
      font_ = Nothing
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

font :: String -> Int -> T -> T
font fontName fontSize term =
   term{font_ = Just (fontName, fontSize)}

{- |
Embed a font file in the generated PostScript output.
Each call adds a new font file,
there is no way to remove it again.
-}
embedFont :: FilePath -> T -> T
embedFont fontFile term =
   term{embedFont_ = fontFile : embedFont_ term}


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
            (maybeToList $ fmap formatMode $ mode_ term) ++
            (maybeToList $ fmap (\b -> if b then "color" else "monochrome") $ color_ term) ++
            (concatMap (\path -> "fontfile" : quote path : []) $ embedFont_ term) ++
            (maybe [] (\(name,size) -> "font" : quote name : show size : []) $ font_ term) ++
            [],
         Terminal.commands =
            ["set output " ++ (quote $ filename_ term)]
      }
