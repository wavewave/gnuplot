module Graphics.Gnuplot.Private.Graph where

class C graph where
   command :: graph -> String
   toString :: graph -> String
