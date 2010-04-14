module Graphics.Gnuplot.Private.Graph3DType where

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import Prelude hiding (lines, )


newtype T x y z a = Cons String

tupleSize :: (Tuple.C a) => T x y z a -> Int
tupleSize =
   let size :: (Tuple.C a) => T x y z a -> (Int, a)
       size _ = Tuple.number
   in  fst . size


{-
See info:/gnuplot/set_style_rectangle
-}

impulses       :: (Atom.C x, Atom.C y, Atom.C z) => T x y z (x,y,z)
vectors        :: (Atom.C x, Atom.C y, Atom.C z) => T x y z ((x,y,z), (x,y,z))
pm3d           :: (Atom.C x, Atom.C y, Atom.C z) => T x y z (x,y,z)

impulses       = Cons "impulses"
vectors        = Cons "vectors"
pm3d           = Cons "pm3d"


toString :: T x y z a -> String
toString (Cons t) = t
