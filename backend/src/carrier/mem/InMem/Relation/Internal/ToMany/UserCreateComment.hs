{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- User create Comment Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ToMany.UserCreateComment where

import Data.Domain (Domain (Comment, User))
import Data.Storage.Map.HasStorage (IdOf)
import InMem.Relation.Internal.ToMany (ToMany (..))

-- | @since 0.3.0.0
data UserCreateComment

-- | @since 0.3.0.0
instance ToMany UserCreateComment where
  type ToManyKey UserCreateComment = IdOf 'User
  type ToManyValue UserCreateComment = IdOf 'Comment
