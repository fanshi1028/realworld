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
import OptionalAuthAction (OptionalAuthActionE (GetArticle))
import OptionalAuthAction.Many (OptionalAuthActionManyE (GetComments, ListArticles))
import Paging (Limit, Offset, Paging (LimitOffset), paging)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Types.SourceT (source)
import Storage.Map (IdOf)
import Util.JSON.To (Out (Out))
import Util.Validation (ValidationErr)
import Validation (Validation (Failure, Success), validation)

-- * API

-- | @since 0.3.0.0
type ArticleApi =
  QP "tag" :> QP "author" :> QP "favorited" :> QP "limit" :> QP "offset" :> ReadManyApi (ArticleR "withAuthorProfile")
    :<|> Cap "slug" (IdOf 'Article)
      :> ( ReadApi (ArticleR "withAuthorProfile")
             :<|> "comments" :> ReadManyApi (CommentR "withAuthorProfile")
         )

-- * Server

-- | @since 0.3.0.0
articleServer ::
  ( Algebra sig m,
    Member OptionalAuthActionE sig,
    Member (OptionalAuthActionManyE []) sig,
    Member (Throw ValidationErr) sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig
  ) =>
  ServerT ArticleApi m
articleServer =
  ( \mTag mAuthor mFavBy mLimit mOffset -> do
      let getVPaging =
            liftA2 LimitOffset
              <$> (R.ask <&> \lim -> fromMaybe (pure lim) mLimit)
              <*> (R.ask <&> \off -> fromMaybe (pure off) mOffset)
          la =
            getVPaging >>= \vp ->
              validation
                (throwError @ValidationErr)
                (\(act, p) -> paging p <$> send act)
                (liftA2 (,) (ListArticles @[] <$> sequenceA mTag <*> sequenceA mAuthor <*> sequenceA mFavBy) vp)
       in Out <$> la :<|> (source <$> la)
  )
    :<|> ( \case
             Success aid ->
               Out <$> send (GetArticle aid)
                 :<|> ( let gc = send $ GetComments aid
                         in Out <$> gc :<|> (source <$> gc)
                      )
             Failure err -> throwError err :<|> throwError err :<|> throwError err
         )
