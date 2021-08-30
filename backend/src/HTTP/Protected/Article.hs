{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) fanshi1028 , 2021
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
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.Util.Error (ValidationErr)
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (Cap, CreateApi, QP, ReadManyApi, ToggleApi, UDApi)
import Servant (Delete, JSON, NoContent (NoContent), ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified UserAction (E (AddCommentToArticle, CreateArticle, DeleteArticle, DeleteComment, FavoriteArticle, FeedArticles, UnfavoriteArticle, UpdateArticle))
import Validation (Validation (Failure, Success))

-- |  @since 0.1.0.0
type CommentApi =
  Cap "id" (CommentR "id") :> Delete '[JSON] NoContent
    :<|> CreateApi CommentR "withAuthorProfile"

-- | @since 0.1.0.0
type FavoriteApi = ToggleApi ArticleR "withAuthorProfile"

-- | @since 0.1.0.0
type ArticleApi =
  CreateApi ArticleR "withAuthorProfile"
    :<|> "feed" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleR "withAuthorProfile"
    :<|> ( Cap "slug" (ArticleR "id")
             :> ( UDApi ArticleR "withAuthorProfile"
                    :<|> "comments" :> CommentApi
                    :<|> "favorite" :> FavoriteApi
                )
         )

-- | @since 0.1.0.0
articleServer ::
  ( Algebra sig m,
    Member UserAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ArticleApi m
articleServer =
  let fromUnValidatedInput f = \case
        In (Failure err) -> throwError err
        In (Success r) -> f r
   in fromUnValidatedInput (Out <<$>> send . UserAction.CreateArticle)
        -- FIXME
        :<|> (\_ _ -> Out <$> send UserAction.FeedArticles)
        :<|> ( \case
                 Success aid ->
                   ( (\(In au) -> Out <$> send (UserAction.UpdateArticle aid au))
                       :<|> send (UserAction.DeleteArticle aid) $> NoContent
                   )
                     -- Comment
                     :<|> ( \case
                              Success cid -> send (UserAction.DeleteComment aid cid) $> NoContent
                              Failure err -> throwError err
                              :<|> fromUnValidatedInput (Out <<$>> send . UserAction.AddCommentToArticle aid)
                          )
                     -- Favourite
                     :<|> ( (Out <$> send (UserAction.FavoriteArticle aid))
                              :<|> (Out <$> send (UserAction.UnfavoriteArticle aid))
                          )
                 Failure err ->
                   (const (throwError err) :<|> throwError err)
                     :<|> ( (const (throwError err) :<|> const (throwError err))
                              :<|> (throwError err :<|> throwError err)
                          )
             )
