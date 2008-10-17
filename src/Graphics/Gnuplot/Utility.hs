module Graphics.Gnuplot.Utility where

import Data.List (intersperse, )


dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p =
   foldr (\x xs -> if p x && null xs then [] else x:xs) []

functionToGraph :: [a] -> (a -> a) -> [(a,a)]
functionToGraph args f = map (\x -> (x, f x)) args
-- functionToGraph args f = map swap $ attachKey f args


showTriplet :: (Show a, Show b, Show c) => (a,b,c) -> String
showTriplet (x,y,z) = unwords [show x, show y, show z]


commaConcat, semiColonConcat :: [String] -> String
commaConcat = concat . intersperse ", "
semiColonConcat = concat . intersperse "; "


quote :: String -> String
quote str = "\"" ++ str ++ "\""




mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f ~(a,b) = (f a, b)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f ~(a,b) = (a, f b)
