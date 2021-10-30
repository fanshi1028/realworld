{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Type family for storage
--
-- @since 0.2.0.0
module Storage.Map.Internal.HasStorage where

import Domain (Domain)

-- | @since 0.2.0.0
class HasStorage (s :: Domain) where
  -- | @since 0.2.0.0
  -- Type for id in storage
  data IdOf s

  -- | @since 0.2.0.0
  -- Type for content in storage
  data ContentOf s

-- * Error

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
