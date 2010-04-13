{- |
Provide a class that restricts the range of Haskell types
to the ones that gnuplot can process.
-}
module Graphics.Gnuplot.Value.Atom where

import Data.Word (Word8, Word16, Word32, Word64, )
import Data.Int (Int8, Int16, Int32, Int64, )
import Data.Ratio (Ratio, )


class C a where

instance C Float   where
instance C Double  where
instance C Int     where
instance C Integer where
instance (Integral a) => C (Ratio a) where

instance C Int8  where
instance C Int16 where
instance C Int32 where
instance C Int64 where
instance C Word8  where
instance C Word16 where
instance C Word32 where
instance C Word64 where
