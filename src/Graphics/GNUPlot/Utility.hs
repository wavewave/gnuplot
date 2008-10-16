module Graphics.GNUPlot.Utility where

dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev p =
   foldr (\x xs -> if p x && null xs then [] else x:xs) []

functionToGraph :: [a] -> (a -> a) -> [(a,a)]
functionToGraph args f = map (\x -> (x, f x)) args

-- functionToGraph args f = map swap $ attachKey f args


mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f ~(a,b) = (f a, b)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f ~(a,b) = (a, f b)
