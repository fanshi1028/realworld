{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of optionally authed action
--
-- @since 0.3.0.0
module OptionalAuthAction where

import Domain (Domain (Article, User))
import Domain.Article (ArticleWithAuthorProfile)
import Domain.User (UserProfile)
import Storage.Map (IdOf)

-- | @since 0.3.0.0
-- Optionally authed actions that can be carried out by visitors or users with different behaviours.
data OptionalAuthActionE (m :: Type -> Type) a where
  -- | @since 0.4.0.0
  -- Get the profile of the user specified by the id.
  GetProfile :: IdOf 'User -> OptionalAuthActionE m UserProfile
  -- | @since 0.4.0.0
  -- Get the article specified by the id.
  GetArticle :: IdOf 'Article -> OptionalAuthActionE m ArticleWithAuthorProfile
