module Graphics.Gnuplot.Terminal where

data T =
   Cons {
      options :: [String],
      commands :: [String]
   }

class C terminal where
   canonical :: terminal -> T
