module Graphics.Gnuplot.Private.Command where

import qualified Graphics.Gnuplot.Private.File as File
import qualified Graphics.Gnuplot.Execute as Exec
import System.Exit (ExitCode, )


run ::
   (File.C file) =>
   [file] -> [String] -> IO ExitCode
run files cmds = do
   mapM_ File.write files
   Exec.simple cmds ["--persist"]
