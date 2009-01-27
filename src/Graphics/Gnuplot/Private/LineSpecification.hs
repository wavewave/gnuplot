module Graphics.Gnuplot.Private.LineSpecification where

import Data.Maybe (catMaybes, )
import Graphics.Gnuplot.Utility (quote, )


data T =
   Cons
      { lineStyle :: Maybe Int
      , lineType  :: Maybe Int
      , lineWidth :: Maybe Double
      , pointType :: Maybe Int
      , pointSize :: Maybe Double
      , title     :: Maybe String
      }

deflt :: T
deflt =
   Cons
      { lineStyle = Nothing
      , lineType  = Nothing
      , lineWidth = Nothing
      , pointType = Nothing
      , pointSize = Nothing
      , title     = Nothing
      }

toString :: T -> String
toString linespec =
   let showField :: String -> (a -> String) -> (T -> Maybe a) -> Maybe [String]
       showField s f access = fmap (\a -> [s, f a]) $ access linespec
   in  unwords $ concat $ catMaybes $
       showField "linetype"  show  lineType  :
       showField "linewidth" show  lineWidth :
       showField "pointtype" show  pointType :
       showField "pointsize" show  pointSize :
       showField "title"     quote title     :
       showField "linestyle" show  lineStyle :
       []
