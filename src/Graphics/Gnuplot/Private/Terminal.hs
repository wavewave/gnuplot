module Graphics.Gnuplot.Private.Terminal where

data T =
   Cons {
      options :: [String],
      commands :: [String]
   }

class C terminal where
   canonical :: terminal -> T

deflt :: T
deflt = Cons ["x11"] []
