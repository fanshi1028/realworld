{-# LANGUAGE DataKinds #-}

-- |
module Relation.Batch.InMem.STM where

import GHC.TypeLits (Symbol)

data E (r1 :: Symbol -> Type) (r2 :: Symbol -> Type) (f :: Type -> Type) (m :: Type -> Type) a where
  GetRelated :: E r1 r2 f m (r1 "id" -> STM (f (r2 "id")))
