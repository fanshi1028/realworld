{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of user's action (many)
--
-- @since 0.3.0.0
module UserAction.Many where

import Domain.Article (ArticleR)

-- | @since 0.3.0.0
-- Actions that can only be carried out by __authenticated__ users.
data UserActionManyE (f :: Type -> Type) (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Get articles feed recommendated for the authenticated user.
  FeedArticles :: UserActionManyE f m (f (ArticleR "withAuthorProfile"))
