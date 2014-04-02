{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Foreign.Marshals
       (
         Marshal(..)
       ) where

import Foreign (free)
import Foreign.C
import GHC.IO (unsafePerformIO)

import Foreign.Marshals.Class


instance Marshal CInt where
  type Haskell CInt = Int
  toHaskell   = return . fromIntegral
  fromHaskell = return . fromIntegral

instance Marshal CString where
  type Haskell CString = String
  toHaskell   = peekCString
  fromHaskell = newCString
  withHaskell = withCString
  cleanup     = free

-- | Bind each type of foreign function to Haskell type
type family MarshalEach f where
  MarshalEach (a -> b) = Haskell a -> MarshalEach b
  MarshalEach (IO b)   = IO (Haskell b)

-- | Marshling result
data Marshaled a = Marshaled { runMarshal :: a }

-- | Marshal each type of a given function `f`
class Marshals f where
  marshalEach :: f -> Marshaled (MarshalEach f)

-- | Marshal types of foreign function's arguments
instance (Marshal a, Marshals b) => Marshals (a -> b) where
  marshalEach f = Marshaled $! \x -> unsafePerformIO
    $ withHaskell x $ return . runMarshal . marshalEach . f
