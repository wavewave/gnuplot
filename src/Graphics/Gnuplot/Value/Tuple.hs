{- |
Provide a class that renders multiple Haskell values in a text form
that is accessible by gnuplot.

Maybe we add a method for the binary interface to gnuplot later.
-}
module Graphics.Gnuplot.Value.Tuple
   (C(text, number),
   ) where

import Data.Word (Word8, Word16, Word32, Word64, )
import Data.Int (Int8, Int16, Int32, Int64, )
import Data.Ratio (Ratio, )


class C a where
   text :: a -> [ShowS]
   {- |
   The right pair member is a dummy result
   that is needed only for type inference.
   Expect that it is undefined
   -}
   number :: (Int, a)
   number = (1, error "gnuplot: dummy atomic value")


singleton :: a -> [a]
singleton = (:[])


instance C Float   where text = singleton . shows
instance C Double  where text = singleton . shows
instance C Int     where text = singleton . shows
instance C Integer where text = singleton . shows
instance (Integral a) => C (Ratio a) where
   text = singleton . shows . (id :: Double->Double) . realToFrac

instance C Int8  where text = singleton . shows
instance C Int16 where text = singleton . shows
instance C Int32 where text = singleton . shows
instance C Int64 where text = singleton . shows
instance C Word8  where text = singleton . shows
instance C Word16 where text = singleton . shows
instance C Word32 where text = singleton . shows
instance C Word64 where text = singleton . shows

instance (C a, C b) => C (a,b) where
   text (a,b) = text a ++ text b
   number =
      let (na,a) = number
          (nb,b) = number
      in  (na+nb, (a,b))

instance (C a, C b, C c) => C (a,b,c) where
   text (a,b,c) = text a ++ text b ++ text c
   number =
      let (na,a) = number
          (nb,b) = number
          (nc,c) = number
      in  (na+nb+nc, (a,b,c))

instance (C a, C b, C c, C d) => C (a,b,c,d) where
   text (a,b,c,d) = text a ++ text b ++ text c ++ text d
   number =
      let (na,a) = number
          (nb,b) = number
          (nc,c) = number
          (nd,d) = number
      in  (na+nb+nc+nd, (a,b,c,d))
