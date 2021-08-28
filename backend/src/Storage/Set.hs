-- |
-- Description : Effect
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect to store data as a set
--
-- @since 0.1.0.0
module Storage.Set (E (..)) where

-- | @since 0.1.0.0
data E (e :: Type) (m :: Type -> Type) k where
  -- | Check if the data is in the storage
  IsMember :: e -> E e m Bool
  -- | Get all the data
  GetAll :: E e m [e]
  -- | Insert the data
  Insert :: e -> E e m ()
  -- | Delete the data from the storage
  Delete :: e -> E e m ()
