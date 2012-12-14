module Graphics.Gnuplot.Private.Terminal where

data T =
   Cons {
      options :: [String],
      commands :: [String]
   }

class C terminal where
   canonical :: terminal -> T

format :: (C terminal) => terminal -> [String]
format term =
   case canonical term of
      Cons opts cmds ->
         if null opts
           then cmds
           else (unwords $ "set" : "terminal" : opts) : cmds
