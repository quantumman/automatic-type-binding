{-# LANGUAGE TypeFamilies #-}

module Foreign.Marshals
       (
         Marshal(..)
       ) where

import Foreign.C (CInt)

import Foreign.Marshals.Class


instance Marshal CInt where
  type Haskell CInt = Int
  toHaskell   = return . fromIntegral
  fromHaskell = return . fromIntegral
