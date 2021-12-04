{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Favorite Relation
--
-- @since 0.3.0.0
module Relation.Internal.ManyToMany.Favorite where

import Domain (Domain (Article, User))
import Relation.Internal.ManyToMany (ManyToMany (..))
import Relation.ToMany (ToMany (..))
import Storage.Map (IdOf)

-- | @since 0.3.0.0
instance ToMany "UserFavoriteArticle" where
  type ToManyKey "UserFavoriteArticle" = IdOf 'User
  type ToManyValue "UserFavoriteArticle" = IdOf 'Article

-- | @since 0.3.0.0
instance ToMany "ArticleFavoritedByUser" where
  type ToManyKey "ArticleFavoritedByUser" = IdOf 'Article
  type ToManyValue "ArticleFavoritedByUser" = IdOf 'User

-- | @since 0.3.0.0
instance ManyToMany "UserFavoriteArticle" where
  type ManyLeft "UserFavoriteArticle" = "UserFavoriteArticle"
  type ManyRight "UserFavoriteArticle" = "ArticleFavoritedByUser"
