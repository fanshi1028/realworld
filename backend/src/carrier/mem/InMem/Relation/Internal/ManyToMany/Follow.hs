{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Follow Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ManyToMany.Follow where

import Data.Domain (Domain (User))
import Data.Storage.Map.HasStorage (IdOf)
import InMem.Relation.Internal.ManyToMany (ManyToMany (..))
import InMem.Relation.Internal.ToMany (ToMany (..))

-- | @since 0.3.0.0
data UserFollowUser

-- | @since 0.3.0.0
data UserFollowedByUser

-- | @since 0.3.0.0
instance ToMany UserFollowUser where
  type ToManyKey UserFollowUser = IdOf 'User
  type ToManyValue UserFollowUser = IdOf 'User

-- | @since 0.3.0.0
instance ToMany UserFollowedByUser where
  type ToManyKey UserFollowedByUser = IdOf 'User
  type ToManyValue UserFollowedByUser = IdOf 'User

-- | @since 0.3.0.0
instance ManyToMany UserFollowUser where
  type ManyLeft UserFollowUser = UserFollowUser
  type ManyRight UserFollowUser = UserFollowedByUser
