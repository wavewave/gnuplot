module Graphics.Gnuplot.Terminal where

data T =
   Cons {
      options :: [String],
      commands :: [String]
   }

class C terminal where
   canonical :: terminal -> T

deflt :: T
deflt = Cons ["x11"] []


formatBool :: String -> Bool -> String
formatBool name b =
   if b then name else "no"++name
