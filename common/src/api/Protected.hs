{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Auth protected API
--
-- @since 0.4.0.0
module API.Protected where

import API.Util (Cap, CreateApi, QP, ReadApi, ReadManyApi, ToggleApi, UDApi, UpdateApi)
import Data.Domain (Domain (Article, Comment, User))
import Data.Domain.Article (ArticleWithAuthorProfile)
import Data.Domain.Comment (CommentWithAuthorProfile)
import Data.Domain.User (UserAuthWithToken, UserProfile)
import Data.Storage.Map (IdOf)
import Servant (Delete, JSON, NoContent, type (:<|>), type (:>))

-- * API

-- |  @since 0.4.0.0
type ProtectedCommentApi =
  Cap "id" (IdOf 'Comment) :> Delete '[JSON] NoContent
    :<|> CreateApi 'Comment CommentWithAuthorProfile

-- | @since 0.4.0.0
type ProtectedFavoriteApi = ToggleApi ArticleWithAuthorProfile

-- | @since 0.4.0.0
type ProtectedArticleApi =
  CreateApi 'Article ArticleWithAuthorProfile
    :<|> "feed" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleWithAuthorProfile
    :<|> ( Cap "slug" (IdOf 'Article)
             :> ( UDApi 'Article ArticleWithAuthorProfile
                    :<|> "comments" :> ProtectedCommentApi
                    :<|> "favorite" :> ProtectedFavoriteApi
                )
         )

-- | @since 0.4.0.0
type ProtectedFollowApi = Cap "username" (IdOf 'User) :> "follow" :> ToggleApi UserProfile

-- | @since 0.4.0.0
type ProtectedUserApi = ReadApi UserAuthWithToken :<|> UpdateApi 'User UserAuthWithToken

-- | @since 0.1.0.0
type ProtectedApi =
  "user" :> ProtectedUserApi
    :<|> "profiles" :> ProtectedFollowApi
    :<|> "articles" :> ProtectedArticleApi
