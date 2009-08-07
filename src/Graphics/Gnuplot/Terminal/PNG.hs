module Graphics.Gnuplot.Terminal.PNG (
   T, cons,
   transparent, noTransparent,
   interlace, noInterlace,
   trueColor, noTrueColor,
   fontTiny, fontSmall, fontMedium, fontLarge, fontGiant,
   ) where

import qualified Graphics.Gnuplot.Terminal as Terminal
import Graphics.Gnuplot.Terminal (formatBool, )
import Data.Maybe (catMaybes, )
import Graphics.Gnuplot.Utility (quote, )


data T =
   Cons {
      filename_ :: FilePath,
      transparent_ :: Maybe Bool,
      interlace_ :: Maybe Bool,
      trueColor_ :: Maybe Bool,
      fontSize_ :: Maybe FontSize
   }

cons :: FilePath -> T
cons path =
   Cons {
      filename_ = path,
      transparent_ = Nothing,
      interlace_ = Nothing,
      trueColor_ = Nothing,
      fontSize_ = Nothing
   }



transparent, noTransparent :: T -> T
transparent   term = term{transparent_ = Just True}
noTransparent term = term{transparent_ = Just False}

interlace, noInterlace :: T -> T
interlace   term = term{interlace_ = Just True}
noInterlace term = term{interlace_ = Just False}

trueColor, noTrueColor :: T -> T
trueColor   term = term{trueColor_ = Just True}
noTrueColor term = term{trueColor_ = Just False}



fontTiny, fontSmall, fontMedium, fontLarge, fontGiant :: T -> T
fontTiny   = setFontSize FontTiny
fontSmall  = setFontSize FontSmall
fontMedium = setFontSize FontMedium
fontLarge  = setFontSize FontLarge
fontGiant  = setFontSize FontGiant



-- private functions

data FontSize =
   FontTiny | FontSmall | FontMedium | FontLarge | FontGiant

formatFontSize :: FontSize -> String
formatFontSize size =
   case size of
      FontTiny   -> "tiny"
      FontSmall  -> "small"
      FontMedium -> "medium"
      FontLarge  -> "large"
      FontGiant  -> "giant"

setFontSize :: FontSize -> T -> T
setFontSize size term =
   term{fontSize_ = Just size}


instance Terminal.C T where
   canonical term =
      Terminal.Cons {
         Terminal.options =
            "png" :
            catMaybes (
               (fmap (formatBool "transparent") $ transparent_ term) :
               (fmap (formatBool "interlace") $ interlace_ term) :
               (fmap (formatBool "truecolor") $ trueColor_ term) :
               (fmap formatFontSize $ fontSize_ term) :
               []),
         Terminal.commands =
            "set" : "output" : (quote $ filename_ term) : []
      }
