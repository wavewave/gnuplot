module Graphics.Gnuplot.File (T(Cons, name, content)) where

import qualified Graphics.Gnuplot.Private.File as File

data T =
   Cons {
      name :: FilePath,
      content :: String
   }
   deriving (Show, Eq)


instance File.C T where
   write (Cons fn cont) = writeFile fn cont
