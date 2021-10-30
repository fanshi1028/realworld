{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect to store data as a map
--
-- @since 0.2.0.0
module Storage.Map
  ( E (..),

    -- * Error

    -- ** Forbidden
    CRUD (..),
    Forbidden (..),

    -- ** Id already exists
    IdAlreadyExists,

    -- ** Id not found
    IdNotFound,

    -- * Typeclass

    -- ** HasStorage
    module X,

    -- ** HasCreate
    module Y,

    -- ** HasUpdate
    module Z,
  )
where

import Domain (Domain)
import Storage.Error (AlreadyExists, NotFound)
import Storage.Map.Internal.HasCreate as Y
import Storage.Map.Internal.HasCreate.Article as Y
import Storage.Map.Internal.HasCreate.Comment as Y
import Storage.Map.Internal.HasCreate.User as Y
import Storage.Map.Internal.HasStorage as X
import Storage.Map.Internal.HasStorage.Article as X
import Storage.Map.Internal.HasStorage.Comment as X
import Storage.Map.Internal.HasStorage.User as X
import Storage.Map.Internal.HasUpdate as Z
import Storage.Map.Internal.HasUpdate.Article as Z
import Storage.Map.Internal.HasUpdate.User as Z

-- | @since 0.2.0.0
data E s (m :: Type -> Type) k where
  -- | @since 0.2.0.0
  -- Get the data from the storage by its id
  GetById :: IdOf s -> E s m (ContentOf s)
  -- | @since 0.2.0.0
  -- Get content the data
  GetAll :: E s m [ContentOf s]
  -- | @since 0.2.0.0
  -- Insert the data
  Insert :: IdOf s -> ContentOf s -> E s m ()
  -- | @since 0.2.0.0
  -- Update the data in the storage by its id
  UpdateById :: IdOf s -> (ContentOf s -> ContentOf s) -> E s m (ContentOf s)
  -- | @since 0.2.0.0
  -- Delete the data from the storage by its id
  DeleteById :: IdOf s -> E s m ()

-- | @since 0.2.0.0
-- see 'Forbidden'
data CRUD
  = -- | @since 0.2.0.0
    -- create
    C
  | -- | @since 0.2.0.0
    -- read
    R
  | -- | @since 0.2.0.0
    -- update
    U
  | -- | @since 0.2.0.0
    -- delete
    D
  deriving (Show)

-- | @since 0.2.0.0
-- error of 'CRUD' action forbiddened on 'Domain'
newtype Forbidden (crud :: CRUD) (a :: Domain) = Forbidden (IdOf a)

-- | @since 0.2.0.0
deriving instance Show (IdOf a) => Show (Forbidden crud a)

-- | @since 0.2.0.0
-- convienient type alias
type IdAlreadyExists a = AlreadyExists (IdOf a)

-- | @since 0.2.0.0
-- convienient type alias
type IdNotFound a = NotFound (IdOf a)
