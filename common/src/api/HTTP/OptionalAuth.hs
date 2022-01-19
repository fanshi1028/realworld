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
-- @since 0.3.0.0
module HTTP.OptionalAuth where

import Domain (Domain (Article, User))
import Domain.Article (ArticleWithAuthorProfile)
import Domain.Comment (CommentWithAuthorProfile)
import Domain.User (UserProfile)
import HTTP.Util (Cap, QP, ReadApi, ReadManyApi)
import Servant (type (:<|>), type (:>))
import Storage.Map (IdOf)

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
