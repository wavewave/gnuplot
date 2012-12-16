module Graphics.Gnuplot.Private.Terminal where

data T =
   Cons {
      options :: [String],
      commands :: [String],
      interactive :: Bool
   }

class C terminal where
   canonical :: terminal -> T

format :: T -> [String]
format (Cons opts cmds _ia) =
   if null opts
     then cmds
     else (unwords $ "set" : "terminal" : opts) : cmds
