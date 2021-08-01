{-# LANGUAGE DataKinds #-}

-- |
module Relation where

import GHC.TypeLits (Symbol)

data E (r1 :: Symbol -> Type) (idx1 :: Symbol) (r2 :: Symbol -> Type) (idx2 :: Symbol) (f :: Type -> Type) (m :: Type -> Type) a where
  Relate :: r1 idx1 -> r2 idx2 -> E r1 idx1 r2 idx2 f m ()
  Unrelate :: r1 idx1 -> r2 idx2 -> E r1 idx1 r2 idx2 f m ()
  IsRelated :: r1 idx1 -> r2 idx2 -> E r1 idx1 r2 idx2 f m Bool
  GetRelated :: r1 idx1 -> E r1 idx1 r2 idx2 f m (f (r2 idx2))
