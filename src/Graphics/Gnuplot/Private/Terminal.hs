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
         ("set terminal " ++ unwords opts) : cmds


deflt :: T
deflt = Cons ["x11"] []
