{-# LANGUAGE DataKinds #-}

-- |
module Relation.OneToMany where

import GHC.TypeLits (Symbol)

data E (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a where
  Relate :: Proxy r -> r1 -> r2 -> E r1 r r2 m ()
  Unrelate :: Proxy r -> r1 -> r2 -> E r1 r r2 m ()
  IsRelated :: Proxy r -> r1 -> r2 -> E r1 r r2 m Bool
  GetRelated :: Proxy r -> r1 -> E r1 r r2 m [r2]
