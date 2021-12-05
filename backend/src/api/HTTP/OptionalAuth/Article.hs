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
module HTTP.OptionalAuth.Article where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain (Domain (Article))
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import HTTP.Util (Cap, QP, ReadApi, ReadManyApi)
import OptionalAuthAction (E (GetArticle, GetComments, ListArticles))
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import InMem.Storage.Map.Internal.HasStorage.User (IdOf)
import Util.JSON.To (Out (Out))
import Util.Validation (ValidationErr)
import Validation (Validation (Failure, Success))

-- * API

-- | @since 0.1.0.0
type ArticleApi =
  QP "tag" :> QP "author" :> QP "favorited" :> QP "limit" :> QP "offset" :> ReadManyApi 'Article (ArticleR "withAuthorProfile")
    :<|> Cap "slug" (IdOf 'Article)
      :> ( ReadApi 'Article (ArticleR "withAuthorProfile")
             :<|> "comments" :> ReadManyApi 'Article (CommentR "withAuthorProfile")
         )

-- * Server

-- | @since 0.1.0.0
articleServer ::
  ( Algebra sig m,
    Member OptionalAuthAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ArticleApi m
articleServer =
  -- FIXME
  (\_ _ _ _ _ -> Out <$> send ListArticles)
    :<|> ( \case
             Success aid ->
               Out <$> send (OptionalAuthAction.GetArticle aid)
                 :<|> (Out <$> send (GetComments aid))
             Failure err -> throwError err :<|> throwError err
         )
