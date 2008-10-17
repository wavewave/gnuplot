module Graphics.Gnuplot.Execute where

import System.Exit (ExitCode, )
import System.Cmd (rawSystem, )


tmpScript :: FilePath
tmpScript = "curve.gp"

simple ::
      [String] {-^ The lines of the gnuplot script to be piped into gnuplot -}
   -> [String] {-^ Options for gnuplot -}
   -> IO ExitCode
simple program options =
   do writeFile tmpScript (unlines program)
      -- putStrLn cmd
      rawSystem "gnuplot" $ options ++ [tmpScript]
