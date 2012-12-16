module Graphics.Gnuplot.Private.Command where

import qualified Graphics.Gnuplot.Private.File as File
import qualified Graphics.Gnuplot.Execute as Exec
import System.Exit (ExitCode, )

import System.IO.Temp (withSystemTempDirectory, )


run ::
   (File.C file) =>
   (FilePath -> ([file], [String])) -> IO ExitCode
run render =
   withSystemTempDirectory "gnuplot" $ \dir ->
   case render dir of
      (files, cmds) -> do
         mapM_ File.write files
         Exec.simple cmds ["--persist"]
         -- instead of the option, one can also use 'set terminal x11 persist'
