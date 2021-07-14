{-# LANGUAGE DataKinds #-}

-- |
module Storage.Index where

import GHC.TypeLits (Symbol)

data E (r :: Symbol -> Type) (idx :: Symbol) (f :: Type -> Type) (m :: Type -> Type) a where
  AddIndex :: r idx -> r "id" -> E r idx f m (r idx)
  GetByIndex :: r idx -> E r idx f m (f (r "id"))
