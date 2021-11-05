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
-- Schema for user_follow_user relation
--
-- @since 0.4.0.0
module InRel8.Storage.Schema.UserFollowUser where

import Domain (Domain (User))
import Field.Time (Time)
import InRel8.Storage.Internal.Field ()
import InRel8.Storage.Schema.Util (snakeNamesFromLabels)
import Rel8 (Column, Name, Rel8able, Result, TableSchema (TableSchema))
import Storage.Map (IdOf)

-- | @since 0.4.0.0
data UserFollowUserRel8 f = UserFollowUserRel8
  { following :: Column f (IdOf 'User),
    followedBy :: Column f (IdOf 'User),
    createdAt :: Column f Time -- "2016-02-18T03:22:56.637Z",
  }
  deriving (Generic)

-- | @since 0.4.0.0
instance Rel8able UserFollowUserRel8

-- | @since 0.4.0.0
deriving instance Show (UserFollowUserRel8 Result)

-- | @since 0.4.0.0
userFollowUserSchema :: TableSchema (UserFollowUserRel8 Name)
userFollowUserSchema = TableSchema "user_follow_user" Nothing $ snakeNamesFromLabels @(UserFollowUserRel8 Name)
