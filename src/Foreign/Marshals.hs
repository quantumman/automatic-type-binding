{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Foreign.Marshals
       (
         Marshal(..)
       ) where

import Foreign (free)
import Foreign.C

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
