{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Schema
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Schema for user
--
-- @since 0.4.0.0
module InRel8.Storage.Schema.User where

import Data.Domain (Domain (User))
import Data.Field.Bio (Bio)
import Data.Field.Email (Email)
import Data.Field.Image (Image)
import Data.Field.Password (PasswordHash)
import Data.Field.Time (Time)
import Data.Storage.Map.HasStorage (IdOf)
import InRel8.Storage.Internal.Field ()
import InRel8.Storage.Schema.Util (snakeNamesFromLabels)
import Rel8 (Column, Name, Rel8able, Result, TableSchema (TableSchema))

-- | @since 0.4.0.0
data UserRel8 f = UserRel8
  { username :: Column f (IdOf 'User),
    email :: Column f Email,
    password :: Column f PasswordHash,
    bio :: Column f Bio,
    image :: Column f Image,
    createdAt :: Column f Time,
    updatedAt :: Column f Time
  }
  deriving (Generic)

-- | @since 0.4.0.0
instance Rel8able UserRel8

-- | @since 0.4.0.0
deriving instance Show (UserRel8 Result)

-- | @since 0.4.0.0
userSchema :: TableSchema (UserRel8 Name)
userSchema = TableSchema "accounts" Nothing $ snakeNamesFromLabels @(UserRel8 Name)
