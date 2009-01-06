module Graphics.Gnuplot.Utility where

import Data.List (intersperse, )


functionToGraph :: [a] -> (a -> a) -> [(a,a)]
functionToGraph args f = map (\x -> (x, f x)) args
-- functionToGraph args f = map swap $ attachKey f args


showTriplet :: (Show a, Show b, Show c) => (a,b,c) -> String
showTriplet (x,y,z) = unwords [show x, show y, show z]


commaConcat, semiColonConcat :: [String] -> String
commaConcat = concat . intersperse ", "
semiColonConcat = concat . intersperse "; "


quote :: String -> String
quote = show
