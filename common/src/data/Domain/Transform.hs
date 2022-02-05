{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Transforms between different representations of Domain Types.
--
-- @since 0.4.0.0
module Data.Domain.Transform (Transform (transform)) where

import Data.Authentication.HasAuth (AuthOf (UserAuth))
import Data.Domain (Domain (Article, Comment, User))
import Data.Storage.Map (ContentOf (..), IdOf, toArticleId, toUserId)
import GHC.Records (getField)

-- | @since 0.2.0.0
-- Transform between representation of data
class Transform a b where
  transform :: a -> b

-- | @since 0.2.0.0
instance Transform (ContentOf 'User) (AuthOf 'User) where
  transform (UserContent em _ name bio' img) = UserAuth em name bio' img

-- | @since 0.2.0.0
instance Transform (ContentOf 'User) (IdOf 'User) where
  transform = toUserId

-- | @since 0.2.0.0
instance Transform (ContentOf 'Article) (IdOf 'Article) where
  transform = toArticleId

-- | @since 0.2.0.0
instance Transform (ContentOf 'Comment) (IdOf 'Comment) where
  transform = getField @"id"
