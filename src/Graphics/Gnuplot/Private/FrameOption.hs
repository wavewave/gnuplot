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
view   :: T; view   = Cons "view"

xRange :: T; xRange = Cons "xrange"
yRange :: T; yRange = Cons "yrange"
zRange :: T; zRange = Cons "zrange"

xLabel :: T; xLabel = Cons "xlabel"
yLabel :: T; yLabel = Cons "ylabel"
zLabel :: T; zLabel = Cons "zlabel"

xTicks :: T; xTicks = Cons "xtics"
yTicks :: T; yTicks = Cons "ytics"
zTicks :: T; zTicks = Cons "ztics"

xData  :: T; xData  = Cons "xdata"
yData  :: T; yData  = Cons "ydata"
zData  :: T; zData  = Cons "zdata"

xFormat :: T; xFormat = Cons "format x"
yFormat :: T; yFormat = Cons "format y"
zFormat :: T; zFormat = Cons "format z"

timeFmt :: T; timeFmt = Cons "timefmt"
