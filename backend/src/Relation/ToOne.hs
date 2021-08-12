{-# LANGUAGE DataKinds #-}

-- |
module Relation.ToOne (E(..)) where

import GHC.TypeLits (Symbol)

data E (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a where
  Relate :: r1 -> r2 -> E r1 r r2 m ()
  Unrelate :: r1 -> r2 -> E r1 r r2 m ()
  IsRelated :: r1 -> r2 -> E r1 r r2 m Bool
  GetRelated :: r1 -> E r1 r r2 m (Maybe r2)
