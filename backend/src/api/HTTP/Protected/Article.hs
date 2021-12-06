{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server for (all related to __article__)
--
-- 1. __Read__ aritcle feeds(customized for the authed user)
--
-- 2. __CUD__ aritcles of the authed user
--
-- 3. __CD__ comments of the authed user on article
--
-- 4. __Toggle__ authed user's __favority__ on article
--
-- @since 0.1.0.0
module HTTP.Protected.Article
  ( -- * API
    ArticleApi,

    -- * Server
    articleServer,
  )
where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain (Domain (Article, Comment))
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import HTTP.Util (Cap, CreateApi, QP, ReadManyApi, ToggleApi, UDApi)
import Servant (Delete, JSON, NoContent (NoContent), ServerT, type (:<|>) ((:<|>)), type (:>))
import Storage.Map (IdOf)
import UserAction
  ( UserActionE
      ( AddCommentToArticle,
        CreateArticle,
        DeleteArticle,
        DeleteComment,
        FavoriteArticle,
        FeedArticles,
        UnfavoriteArticle,
        UpdateArticle
      ),
  )
import Util.JSON.From (In (In))
import Util.JSON.To (Out (Out))
import Util.Validation (ValidationErr)
import Validation (Validation (Failure, Success))

-- |  @since 0.1.0.0
type CommentApi =
  Cap "id" (IdOf 'Comment) :> Delete '[JSON] NoContent
    :<|> CreateApi 'Comment (CommentR "withAuthorProfile")

-- | @since 0.1.0.0
type FavoriteApi = ToggleApi 'Article (ArticleR "withAuthorProfile")

-- | @since 0.1.0.0
type ArticleApi =
  CreateApi 'Article (ArticleR "withAuthorProfile")
    :<|> "feed" :> QP "limit" :> QP "offset" :> ReadManyApi 'Article (ArticleR "withAuthorProfile")
    :<|> ( Cap "slug" (IdOf 'Article)
             :> ( UDApi 'Article (ArticleR "withAuthorProfile")
                    :<|> "comments" :> CommentApi
                    :<|> "favorite" :> FavoriteApi
                )
         )

-- | @since 0.3.0.0
articleServer ::
  ( Algebra sig m,
    Member UserActionE sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ArticleApi m
articleServer =
  let fromUnValidatedInput f = \case
        In (Failure err) -> throwError err
        In (Success r) -> f r
   in fromUnValidatedInput (Out <<$>> send . CreateArticle)
        -- FIXME
        :<|> (\_ _ -> Out <$> send UserAction.FeedArticles)
        :<|> ( \case
                 Success aid ->
                   ( fromUnValidatedInput (Out <<$>> send . UpdateArticle aid)
                       :<|> send (DeleteArticle aid) $> NoContent
                   )
                     -- Comment
                     :<|> ( \case
                              Success cid -> send (DeleteComment aid cid) $> NoContent
                              Failure err -> throwError err
                              :<|> fromUnValidatedInput (Out <<$>> send . AddCommentToArticle aid)
                          )
                     -- Favourite
                     :<|> ( (Out <$> send (FavoriteArticle aid))
                              :<|> (Out <$> send (UnfavoriteArticle aid))
                          )
                 Failure err ->
                   (const (throwError err) :<|> throwError err)
                     :<|> ( (const (throwError err) :<|> const (throwError err))
                              :<|> (throwError err :<|> throwError err)
                          )
             )
