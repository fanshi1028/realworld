{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Favorite Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ManyToMany.Favorite where

import Data.Domain (Domain (Article, User))
import Data.Storage.Map (IdOf)
import InMem.Relation.Internal.ManyToMany (ManyToMany (..))
import InMem.Relation.Internal.ToMany (ToMany (..))

-- | @since 0.3.0.0
data UserFavoriteArticle

-- | @since 0.3.0.0
data ArticleFavoritedByUser

-- | @since 0.3.0.0
instance ToMany UserFavoriteArticle where
  type ToManyKey UserFavoriteArticle = IdOf 'User
  type ToManyValue UserFavoriteArticle = IdOf 'Article

-- | @since 0.3.0.0
instance ToMany ArticleFavoritedByUser where
  type ToManyKey ArticleFavoritedByUser = IdOf 'Article
  type ToManyValue ArticleFavoritedByUser = IdOf 'User

-- | @since 0.3.0.0
instance ManyToMany UserFavoriteArticle where
  type ManyLeft UserFavoriteArticle = UserFavoriteArticle
  type ManyRight UserFavoriteArticle = ArticleFavoritedByUser
