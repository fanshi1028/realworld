{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server to read articles and comments
--
-- @since 0.1.0.0
module HTTP.Public.Article where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.Util.Error (ValidationErr)
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (Cap, QP, ReadApi, ReadManyApi)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import Validation (Validation (Failure, Success))
import VisitorAction (E (GetArticle, GetComments, ListArticles))

-- * API

-- | @since 0.1.0.0
type ArticleApi =
  QP "tag" :> QP "author" :> QP "favorited" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleR "withAuthorProfile"
    :<|> Cap "slug" (ArticleR "id")
      :> ( ReadApi ArticleR "withAuthorProfile"
             :<|> "comments" :> ReadManyApi CommentR "withAuthorProfile"
         )

-- * Server

-- | @since 0.1.0.0
articleServer ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ArticleApi m
articleServer =
  -- FIXME
  (\_ _ _ _ _ -> Out <$> send ListArticles)
    :<|> ( \case
             Success aid ->
               Out <$> send (VisitorAction.GetArticle aid)
                 :<|> (Out <$> send (GetComments aid))
             Failure err -> throwError err :<|> throwError err
         )
