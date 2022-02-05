{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server, optional authed.
--
-- @since 0.4.0.0
module API.OptionalAuth where

import API.Util (Cap, QP, ReadApi, ReadManyApi)
import Data.Domain (Domain (Article, User))
import Data.Domain.Article (ArticleWithAuthorProfile)
import Data.Domain.Comment (CommentWithAuthorProfile)
import Data.Domain.User (UserProfile)
import Data.Storage.Map (IdOf)
import Servant (type (:<|>), type (:>))

-- * API

-- | @since 0.4.0.0
type OptionalAuthArticleApi =
  QP "tag" :> QP "author" :> QP "favorited" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleWithAuthorProfile
    :<|> Cap "slug" (IdOf 'Article)
      :> (ReadApi ArticleWithAuthorProfile :<|> "comments" :> ReadManyApi CommentWithAuthorProfile)

-- | @since 0.4.0.0
type OptionalAuthProfileApi = Cap "username" (IdOf 'User) :> ReadApi UserProfile

-- | @since 0.3.0.0
type OptionalAuthApi =
  "profiles" :> OptionalAuthProfileApi
    :<|> "articles" :> OptionalAuthArticleApi
