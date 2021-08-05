{-# LANGUAGE DataKinds #-}

-- |
module Relation.Batch where

import GHC.TypeLits (Symbol)

data E (r1 :: Type) (r :: Symbol) (r2 :: Type) (f :: Type -> Type) (m :: Type -> Type) a where
  GetRelated :: Proxy r -> r1 -> E r1 r r2 f m (f r2)

