{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.Marshals
       (
         Marshal(..)
       , marshals
       ) where

import Data.Int
import Data.Word
import Foreign (free)
import Foreign.C
import GHC.IO (unsafePerformIO)

import Foreign.Marshals.Class
import Foreign.Marshals.TH


mkMarshalInstances ''Int8 [''CChar, ''CSChar]
mkMarshalInstances ''Word8 [''CUChar]
mkMarshalInstances ''Int16 [''CShort]
mkMarshalInstances ''Word16 [''CUShort]
mkMarshalInstances ''Int [''CInt, ''CWchar]
mkMarshalInstances ''Word [''CUInt, ''CSize]
mkMarshalInstances ''Int64 [''CLong]

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

-- | Marshal return type of foreign function
instance (Marshal a) => Marshals (IO a) where
  marshalEach = Marshaled . (toHaskell =<<)

-- | Provide `marshals` converting foreign function with foreign types
-- to Haskell function.
--
-- Exmaple.
--
-- The following code is an example to marshal foreign function `put` and
-- use marshaed functon `put'`.
-- "Hello World" should be outputed by `put'`.
--
-- >>> import Foreign.C
-- >>> foreign import ccall puts :: CString -> IO CInt
-- >>> :{
-- do
--   let puts' = marshals puts
--   r <- puts' "Hello World"
--   return $ r > 0
-- :}
-- True
--
-- The type of `puts` is coverted to String -> IO Int
--
-- >>> import Data.Typeable
--
-- >>> show $ typeOf puts
-- "Ptr CChar -> IO CInt"
--
-- >>> let puts' = marshals puts
-- >>> show $ typeOf puts'
-- "[Char] -> IO Int"
marshals :: (Marshals f) => f -> MarshalEach f
marshals = runMarshal . marshalEach
