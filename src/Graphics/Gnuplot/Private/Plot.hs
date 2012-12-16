module Graphics.Gnuplot.Private.Plot where

import qualified Graphics.Gnuplot.Private.Display as Display
import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Private.File as FileClass
import qualified Graphics.Gnuplot.File as File
import Graphics.Gnuplot.Utility (quote, commaConcat, )

import qualified Data.Foldable as Fold
import qualified Data.Monoid.Reader as Reader
import qualified Data.Monoid.State as State
import Data.Monoid (Monoid, mempty, mappend, )

import Data.Maybe (mapMaybe, )

import qualified System.FilePath as Path
import System.FilePath ((</>), )


{- |
Plots can be assembled using 'mappend' or 'mconcat'
or several functions from "Data.Foldable".
-}
newtype T graph = Cons (State.T Int (Reader.T FilePath [File graph]))

pure :: [File graph] -> T graph
pure = Cons . State.pure . Reader.pure

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
   Cons $ State.Cons $ \n ->
      (Reader.Cons $ \dir ->
          [File
             (dir </> Path.addExtension (tmpFileStem ++ show n) "csv")
             (Just content) graphs],
       succ n)

fromGraphs :: FilePath -> [graph] -> T graph
fromGraphs name gs =
   pure [File name Nothing gs]


data File graph =
   File {
      filename_ :: FilePath,
      content_ :: Maybe String,
      graphs_ :: [graph]
   }

instance FileClass.C (File graph) where
   write (File fn cont _) =
      Fold.mapM_ (writeFile fn) cont


tmpFileStem :: FilePath
tmpFileStem = "curve"

{-
tmpFile :: FilePath
tmpFile = tmpFileStem ++ ".csv"
-}


instance Functor T where
   fmap f (Cons mp) =
      Cons $
      fmap (fmap (map (\file -> file{graphs_ = map f $ graphs_ file})))
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
         let (rd, n1) = State.run mp n0
             files blocks =
                mapMaybe
                   (\blk -> fmap (File.Cons (filename_ blk)) (content_ blk))
                   blocks
             graphs blocks =
                concatMap
                   (\blk ->
                      map
                         (\gr ->
                            quote (filename_ blk) ++ " " ++
                            Graph.toString gr) $ graphs_ blk) $
                blocks
         in  (Reader.Cons $ \dir ->
                 case Reader.run rd dir of
                    blocks ->
                       Display.Body (files blocks)
                          [plotCmd p undefined ++ " " ++ commaConcat (graphs blocks)],
              (n1, opts))

optionsToScript :: Graph.C graph => OptionSet.T graph -> Display.Script
optionsToScript opts =
   Display.Script $
      State.Cons $ \(n, opts0) ->
         let opts1 = OptionSet.decons opts
         in  (Reader.pure $ Display.Body [] $
              OptionSet.diffToString opts0 opts1,
              (n, opts1))

defltOpts :: Graph.C graph => T graph -> OptionSet.T graph
defltOpts _ = Graph.defltOptions

instance Graph.C graph => Display.C (T graph) where
   toScript plot =
      optionsToScript (defltOpts plot)  `mappend`  toScript plot

plotCmd ::
   Graph.C graph =>
   T graph -> graph -> String
plotCmd _plot = Graph.command
