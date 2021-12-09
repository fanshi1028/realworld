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
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain (Domain (Article))
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import HTTP.Util (Cap, QP, ReadApi, ReadManyApi)
import OptionalAuthAction (OptionalAuthActionE (GetArticle, GetComments, ListArticles))
import Paging (Limit, Offset, Paging (LimitOffSet), paging)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import Storage.Map (IdOf)
import Util.JSON.To (Out (Out))
import Util.Validation (ValidationErr)
import Validation (Validation (Failure, Success), validation)

-- * API

-- | @since 0.1.0.0
type ArticleApi =
  QP "tag" :> QP "author" :> QP "favorited" :> QP "limit" :> QP "offset" :> ReadManyApi 'Article (ArticleR "withAuthorProfile")
    :<|> Cap "slug" (IdOf 'Article)
      :> ( ReadApi 'Article (ArticleR "withAuthorProfile")
             :<|> "comments" :> ReadManyApi 'Article (CommentR "withAuthorProfile")
         )

-- * Server

-- | @since 0.3.0.0
articleServer ::
  ( Algebra sig m,
    Member OptionalAuthActionE sig,
    Member (Throw ValidationErr) sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig
  ) =>
  ServerT ArticleApi m
articleServer =
  ( \mTag mAuthor mFavBy mLimit mOffset -> do
      vLimit <- R.ask >>= \lim -> pure $ fromMaybe (pure lim) mLimit
      vOffset <- R.ask >>= \off -> pure $ fromMaybe (pure off) mOffset
      Out
        <$> validation
          (throwError @ValidationErr)
          (\(act, p) -> paging p <$> send act)
          ( liftA2
              (,)
              (ListArticles <$> sequenceA mTag <*> sequenceA mAuthor <*> sequenceA mFavBy)
              (LimitOffSet <$> vLimit <*> vOffset)
          )
  )
    :<|> ( \case
             Success aid ->
               Out <$> send (OptionalAuthAction.GetArticle aid)
                 :<|> (Out <$> send (GetComments aid))
             Failure err -> throwError err :<|> throwError err
         )
