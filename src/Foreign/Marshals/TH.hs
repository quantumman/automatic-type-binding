{-# LANGUAGE TemplateHaskell #-}

module Foreign.Marshals.TH
       (
         mkMarshalInstance
       , mkMarshalInstances
       ) where

import Control.Applicative
import Language.Haskell.TH

import Foreign.Marshals.Class

-- Declare instances for c number types, ie. CChar, CShort and so on.
mkMarshalInstance :: Name -> Name -> Q [Dec]
mkMarshalInstance ctype htype = [d|
    instance Marshal $fromT where
      type Haskell $fromT = $toT
      toHaskell   = return . fromIntegral
      fromHaskell = return . fromIntegral
  |]
  where
    fromT = conT ctype
    toT = conT htype

mkMarshalInstances :: Name -> [Name] -> Q [Dec]
mkMarshalInstances htype =
  foldl1 (\x y -> (++) <$> x <*> y)
  . map (flip mkMarshalInstance htype)
