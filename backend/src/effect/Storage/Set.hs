-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect to store data as a set
--
-- @since 0.1.0.0
module Storage.Set (E (..)) where

-- | @since 0.1.0.0
data E (e :: Type) (m :: Type -> Type) k where
  -- | @since 0.1.0.0
  -- Check if the data is in the storage
  IsMember :: e -> E e m Bool
  -- | @since 0.1.0.0
  -- Get all the data
  GetAll :: E e m [e]
  -- | @since 0.1.0.0
  -- Insert the data
  Insert :: e -> E e m ()
  -- | @since 0.1.0.0
  -- Delete the data from the storage
  Delete :: e -> E e m ()
