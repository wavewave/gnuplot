module Graphics.Gnuplot.Private.Terminal where

data T =
   Cons {
      options :: [String],
      commands :: [String]
   }

class C terminal where
   canonical :: terminal -> T

format :: T -> [String]
format (Cons opts cmds) =
   if null opts
     then cmds
     else (unwords $ "set" : "terminal" : opts) : cmds
