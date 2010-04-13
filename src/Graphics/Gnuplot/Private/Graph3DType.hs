module Graphics.Gnuplot.Private.Graph3DType where

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import Prelude hiding (lines, )


newtype T a = Cons String

tupleSize :: (Tuple.C a) => T a -> Int
tupleSize =
   let size :: (Tuple.C a) => T a -> (Int, a)
       size _ = Tuple.number
   in  fst . size


{-
See info:/gnuplot/set_style_rectangle
-}

impulses       :: (Atom.C x, Atom.C y, Atom.C z) => T (x,y,z)
vectors        :: (Atom.C x, Atom.C y, Atom.C z) => T ((x,y,z), (x,y,z))
pm3d           :: (Atom.C x, Atom.C y, Atom.C z) => T (x,y,z)

impulses       = Cons "impulses"
vectors        = Cons "vectors"
pm3d           = Cons "pm3d"


toString :: T a -> String
toString (Cons t) = t
