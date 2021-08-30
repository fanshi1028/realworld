{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect to store data as a map
--
-- @since 0.1.0.0
module Storage.Map (E (..)) where

import GHC.TypeLits (Symbol)

-- | @since 0.1.0.0
data E (r :: Symbol -> Type) (m :: Type -> Type) k where
  -- | Get the data from the storage by its id
  GetById :: r "id" -> E r m (r "all")
  -- | Get all the data
  GetAll :: E r m [r "all"]
  -- | Insert the data
  Insert :: r "all" -> E r m ()
  -- | Update the data in the storage by its id
  UpdateById :: r "id" -> (r "all" -> r "all") -> E r m (r "all")
  -- | Delete the data from the storage by its id
  DeleteById :: r "id" -> E r m ()
