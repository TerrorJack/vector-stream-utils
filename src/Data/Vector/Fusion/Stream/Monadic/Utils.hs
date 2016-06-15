module Data.Vector.Fusion.Stream.Monadic.Utils where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.IOData
import Data.MonoTraversable
import Data.Vector.Fusion.Bundle.Monadic
import Data.Vector.Fusion.Bundle.Size
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Generic
import Data.Vector.Generic.Mutable
import System.IO
import System.Random.MWC

getMVector :: (MVector v a, PrimMonad m) => Stream m a -> m (v (PrimState m) a)
getMVector s = munstream $ fromStream s Unknown

getVector :: (Vector v a, PrimMonad m) => Stream m a -> m (v a)
getVector s = getMVector s >>= unsafeFreeze

constStream :: m (Step () a) -> Stream m a
constStream m = Stream (const m) ()

hGetChunkStream :: (IOData a, MonadIO m) => Handle -> Stream m a
hGetChunkStream h = constStream $ do
    chunk <- hGetChunk h
    pure $ if onull chunk then Done else Yield chunk ()

getRandomStream :: (PrimMonad m, Variate a) => Gen (PrimState m) -> Stream m a
getRandomStream g = constStream $ do
    x <- uniform g
    pure $ Yield x ()

getRandomStreamR :: (PrimMonad m, Variate a) => Gen (PrimState m) -> (a, a) -> Stream m a
getRandomStreamR g r = constStream $ do
    x <- uniformR r g
    pure $ Yield x ()
