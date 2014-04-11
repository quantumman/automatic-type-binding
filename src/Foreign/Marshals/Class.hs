{-# LANGUAGE TypeFamilies #-}

module Foreign.Marshals.Class where

import Control.Exception (bracket)

-- | Provide convert functions for Haskell and C language
-- types without memory leak.
class Marshal a where
  -- | a Haskell type corresponding to a C language type.
  type Haskell a

  -- | Get a value returned by foreign function as a value of Haskell.
  toHaskell   :: a -> IO (Haskell a)

  -- | Give a value typed in Haskell world to foreign function.
  fromHaskell :: Haskell a -> IO a

  -- | Resource safe version for `fromHaskell`.
  -- `withHaskell` may allocate resource for an argument of
  -- foreign function. So this function frees the allocated resource
  -- in termination of processing.
  -- Overloaded `withHaskell` also should ensure to free allocated space.
  withHaskell :: Haskell a -> (a -> IO b) -> IO b
  withHaskell x f = bracket (fromHaskell x) cleanup f

  -- | Cleanup handler is supposed to be invoked in the termination of
  -- `withHaskell`.
  cleanup :: a -> IO ()
  cleanup _ = return ()
