module Graphics.Gnuplot.Private.Plot where

import qualified Data.Monoid.State as State
import Data.Monoid (Monoid, mempty, mappend, )

import qualified Graphics.Gnuplot.Display as Display
import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.Graph as Graph
import Graphics.Gnuplot.Utility (quote, commaConcat, )

import Data.Maybe (mapMaybe, )


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


tmpFileStem :: FilePath
tmpFileStem = "curve"

{-
tmpFile :: FilePath
tmpFile = tmpFileStem ++ ".csv"
-}


instance Functor T where
   fmap f (Cons mp) =
      Cons $
      fmap (map (\file -> file{graphs_ = map f $ graphs_ file}))
      mp

{- |
In contrast to the Display.toScript method instantiation
this function leaves the options,
and thus can be used to write the Display.toScript instance for Frame.
-}
toScript :: Graph.C graph => T graph -> Display.Script
toScript p@(Cons mp) =
   Display.Script $ State.Cons $
      \(n0, opts) ->
         let (blocks, n1) = State.run mp n0
             files =
                mapMaybe
                   (\blk -> fmap (Display.File (filename_ blk)) (content_ blk))
                   blocks
             graphs =
                concatMap (\blk ->
                   map (\gr ->
                           quote (filename_ blk) ++ " " ++
                           Graph.toString gr) $ graphs_ blk) $
                   blocks
         in  (Display.Body files
                 [plotCmd p undefined ++ " " ++ commaConcat graphs],
              (n1, opts))

instance Graph.C graph => Display.C (T graph) where
   toScript plot =
      (Display.Script $
         State.Cons $ \(n, opts0) ->
            let defltOpts :: Graph.C graph => T graph -> OptionSet.T graph
                defltOpts _ = Graph.defltOptions
                opts1 = OptionSet.decons (defltOpts plot)
            in  (Display.Body [] $
                 OptionSet.diffToString opts0 opts1,
                 (n, opts1)))
      `mappend`
      toScript plot

plotCmd ::
   Graph.C graph =>
   T graph -> graph -> String
plotCmd _plot = Graph.command
