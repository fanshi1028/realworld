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
-- Schema for user_favourite_article relation
--
-- @since 0.4.0.0
module InRel8.Storage.Schema.UserFavoriteArticle where

import Data.Domain (Domain (Article, User))
import Data.Field.Time (Time)
import Data.Storage.Map.HasStorage (IdOf)
import InRel8.Storage.Internal.Field ()
import InRel8.Storage.Schema.Util (snakeNamesFromLabels)
import Rel8 (Column, Name, Rel8able, Result, TableSchema (TableSchema))

-- | @since 0.4.0.0
data UserFavoriteArticleRel8 f = UserFavoriteArticleRel8
  { favoritedBy :: Column f (IdOf 'User),
    favoriting :: Column f (IdOf 'Article),
    createdAt :: Column f Time -- "2016-02-18T03:22:56.637Z",
  }
  deriving (Generic)

-- | @since 0.4.0.0
instance Rel8able UserFavoriteArticleRel8

-- | @since 0.4.0.0
deriving instance Show (UserFavoriteArticleRel8 Result)

-- | @since 0.4.0.0
userFavoriteArticleSchema :: TableSchema (UserFavoriteArticleRel8 Name)
userFavoriteArticleSchema = TableSchema "user_favorite_article" Nothing $ snakeNamesFromLabels @(UserFavoriteArticleRel8 Name)
