module Graphics.Gnuplot.Private.Plot where

import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph as Graph

import qualified Data.Monoid.State as State
import Data.Monoid (Monoid, mempty, mappend, )


{- |
Plots can be assembled using 'mappend' or 'mconcat'.
-}
newtype T = Cons (State.T Int [File])

instance Monoid T where
   mempty = Cons mempty
   mappend (Cons s0) (Cons s1) =
      Cons (mappend s0 s1)


withUniqueFile :: String -> [Graph.T] -> T
withUniqueFile content graphs =
   Cons (State.Cons $ \n ->
      ([File (tmpFileStem ++ show n ++ ".dat") (Just content) graphs],
       succ n))

fromGraphs :: FilePath -> [Graph.T] -> T
fromGraphs name gs =
   Cons (State.pure [File name Nothing gs])


data File =
   File {
      filename_ :: FilePath,
      content_ :: Maybe String,
      graphs_ :: [Graph.T]
   }


tmpFileStem, tmpFile :: FilePath

tmpFileStem = "curve"
tmpFile = tmpFileStem ++ ".dat"



mapGraphs :: (Graph.T -> Graph.T) -> T -> T
mapGraphs f (Cons mp) =
   Cons $
   fmap (map (\file -> file{graphs_ = map f $ graphs_ file}))
   mp


typ :: Graph.Type -> T -> T
typ t = mapGraphs (Graph.typ t)

lineSpec :: LineSpec.T -> T -> T
lineSpec ls = mapGraphs (Graph.lineSpec ls)
