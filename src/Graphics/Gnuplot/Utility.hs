module Graphics.Gnuplot.Utility where

import Data.List (intersperse, )


functionToGraph :: [x] -> (x -> y) -> [(x,y)]
functionToGraph args f = map (\x -> (x, f x)) args
-- functionToGraph args f = map swap $ attachKey f args


linearScale :: Fractional a => Integer -> (a,a) -> [a]
linearScale n (x0,x1) =
   map (\m -> x0 + (x1-x0) * fromIntegral m / fromIntegral n) [0..n]


showTriplet :: (Show a, Show b, Show c) => (a,b,c) -> String
showTriplet (x,y,z) = unwords [show x, show y, show z]


commaConcat, semiColonConcat :: [String] -> String
commaConcat = concat . intersperse ", "
semiColonConcat = concat . intersperse "; "


quote :: String -> String
quote = show

assembleCells :: [[ShowS]] -> String
assembleCells ps =
   foldr ($) ""
      (concatMap
         (\p ->
            intersperse (showString ", ") p ++
            [showString "\n"])
         ps)
