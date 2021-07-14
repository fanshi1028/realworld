{-# LANGUAGE DataKinds #-}

-- |
module Storage where

import GHC.TypeLits (Symbol)

data E (r :: Symbol -> Type) (m :: Type -> Type) k where
  GetById :: r "id" -> E r m (Maybe (r "all"))
  GetAll :: E r m [r "all"]
  Insert :: r "create" -> E r m (r "all")
  UpdateById :: r "id" -> (r "all" -> r "all") -> E r m (r "all")
  DeleteById :: r "id" -> E r m ()
