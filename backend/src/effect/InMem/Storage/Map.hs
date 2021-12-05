{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Type
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Type to related to storing data in memory as a map
--
-- @since 0.3.0.0
module InMem.Storage.Map
  ( -- * Error

    -- ** Forbidden
    CRUD (..),
    Forbidden (..),

    -- ** Id already exists
    IdAlreadyExists,

    -- ** Id not found
    IdNotFound,

    -- * Typeclass

    -- ** HasCreate
    module X,

    -- ** HasStorage
    module Y,

    -- ** HasUpdate
    module Z,
  )
where

import Domain (Domain)
import Error.Storage (AlreadyExists, NotFound)
import InMem.Storage.Map.Internal.HasCreate as X
import InMem.Storage.Map.Internal.HasCreate.Article as X
import InMem.Storage.Map.Internal.HasCreate.Comment as X
import InMem.Storage.Map.Internal.HasCreate.User as X
import InMem.Storage.Map.Internal.HasStorage as Y
import InMem.Storage.Map.Internal.HasStorage.Article as Y
import InMem.Storage.Map.Internal.HasStorage.Comment as Y
import InMem.Storage.Map.Internal.HasStorage.User as Y
import InMem.Storage.Map.Internal.HasUpdate as Z
import InMem.Storage.Map.Internal.HasUpdate.Article as Z
import InMem.Storage.Map.Internal.HasUpdate.User as Z

-- | @since 0.3.0.0
-- see 'Forbidden'
data CRUD
  = -- | @since 0.3.0.0
    -- create
    C
  | -- | @since 0.3.0.0
    -- read
    R
  | -- | @since 0.3.0.0
    -- update
    U
  | -- | @since 0.3.0.0
    -- delete
    D
  deriving (Show)

-- | @since 0.3.0.0
-- error of 'CRUD' action forbiddened on 'Domain'
newtype Forbidden (crud :: CRUD) (a :: Domain) = Forbidden (IdOf a)

-- | @since 0.3.0.0
deriving instance Show (IdOf a) => Show (Forbidden crud a)

-- | @since 0.3.0.0
-- convienient type alias
type IdAlreadyExists a = AlreadyExists (IdOf a)

-- | @since 0.3.0.0
-- convienient type alias
type IdNotFound a = NotFound (IdOf a)
