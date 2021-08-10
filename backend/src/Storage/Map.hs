{-# LANGUAGE DataKinds #-}

-- |
module Storage.Map (E (..)) where

import GHC.TypeLits (Symbol)

data E (r :: Symbol -> Type) (m :: Type -> Type) k where
  GetById :: r "id" -> E r m (r "all")
  GetAll :: E r m [r "all"]
  Insert :: r "all" -> E r m ()
  UpdateById :: r "id" -> (r "all" -> r "all") -> E r m (r "all")
  DeleteById :: r "id" -> E r m ()
