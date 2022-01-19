{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Auth protected API & Server
--
-- @since 0.1.0.0
module HTTP.Protected where

import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleWithAuthorProfile)
import Domain.Comment (CommentWithAuthorProfile)
import Domain.User (UserAuthWithToken, UserProfile)
import HTTP.Util (Cap, CreateApi, QP, ReadApi, ReadManyApi, ToggleApi, UDApi, UpdateApi)
import Servant (Delete, JSON, NoContent, type (:<|>), type (:>))
import Storage.Map (IdOf)

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
