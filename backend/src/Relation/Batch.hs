{-# LANGUAGE DataKinds #-}

-- |
module Relation.Batch where

import GHC.TypeLits (Symbol)

data E (r1 :: Symbol -> Type) (r2 :: Symbol -> Type) (f :: Type -> Type) (m :: Type -> Type) a where
  GetRelated :: r1 "id" -> E r1 r2 f m (f (r2 "id"))
