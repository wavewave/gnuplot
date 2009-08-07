module Graphics.Gnuplot.Private.FrameOption where

newtype T = Cons String
   deriving (Eq, Ord, Show)

custom :: String -> T
custom = Cons

title  :: T; title  = Cons "title"
grid   :: T; grid   = Cons "grid"
size   :: T; size   = Cons "size"
key    :: T; key    = Cons "key"
border :: T; border = Cons "border"
pm3d   :: T; pm3d   = Cons "pm3d"

xRange :: T; xRange = Cons "xrange"
yRange :: T; yRange = Cons "yrange"
zRange :: T; zRange = Cons "zrange"

xLabel :: T; xLabel = Cons "xlabel"
yLabel :: T; yLabel = Cons "ylabel"
zLabel :: T; zLabel = Cons "zlabel"

xTicks :: T; xTicks = Cons "xtics"
yTicks :: T; yTicks = Cons "ytics"
zTicks :: T; zTicks = Cons "ztics"
