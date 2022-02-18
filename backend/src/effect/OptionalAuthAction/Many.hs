{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of optionally authed action (many)
--
-- @since 0.4.0.0
module OptionalAuthAction.Many where

import Data.Domain (Domain (Article))
import Data.Domain.Article (ArticleWithAuthorProfile)
import Data.Domain.Comment (CommentWithAuthorProfile)
import Data.Field.Tag (Tag)
import Data.Field.Username (Username)
import Data.Storage.Map (HasStorage (IdOf))

-- | @since 0.3.0.0
-- Optionally authed actions that can be carried out by visitors or users with different behaviours.
data OptionalAuthActionManyE f (m :: Type -> Type) a where
  -- | @since 0.4.0.0
  -- Get all the articles.
  ListArticles :: Maybe Tag -> Maybe Username -> Maybe Username -> OptionalAuthActionManyE f m (f ArticleWithAuthorProfile)
  -- | @since 0.4.0.0
  -- Get all the comments of the article specified by the id.
  GetComments :: IdOf 'Article -> OptionalAuthActionManyE f m (f CommentWithAuthorProfile)
