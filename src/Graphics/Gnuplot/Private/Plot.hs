module Graphics.Gnuplot.Private.Plot where

import qualified Data.Monoid.State as State
import Data.Monoid (Monoid, mempty, mappend, )


{- |
Plots can be assembled using 'mappend' or 'mconcat'.
-}
newtype T graph = Cons (State.T Int [File graph])

{-
Could also be implemented with Control.Monad.Trans.State:
mappend = liftA2 mappend
-}
instance Monoid (T graph) where
   mempty = Cons mempty
   mappend (Cons s0) (Cons s1) =
      Cons (mappend s0 s1)


withUniqueFile :: String -> [graph] -> T graph
withUniqueFile content graphs =
   Cons (State.Cons $ \n ->
      ([File (tmpFileStem ++ show n ++ ".csv") (Just content) graphs],
       succ n))

fromGraphs :: FilePath -> [graph] -> T graph
fromGraphs name gs =
   Cons (State.pure [File name Nothing gs])


data File graph =
   File {
      filename_ :: FilePath,
      content_ :: Maybe String,
      graphs_ :: [graph]
   }

writeData :: File graph -> IO ()
writeData (File fn cont _) =
   maybe (return ()) (writeFile fn) cont


tmpFileStem, tmpFile :: FilePath

tmpFileStem = "curve"
tmpFile = tmpFileStem ++ ".csv"


instance Functor T where
   fmap f (Cons mp) =
      Cons $
      fmap (map (\file -> file{graphs_ = map f $ graphs_ file}))
      mp
