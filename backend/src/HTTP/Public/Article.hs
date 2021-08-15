{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.Article (ArticleApi, articleServer) where

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

type ArticleApi =
  QP "tag" :> QP "author" :> QP "favorited" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleR "withAuthorProfile"
    :<|> Cap "slug" (ArticleR "id")
      :> ( ReadApi ArticleR "withAuthorProfile"
             :<|> "comments" :> ReadManyApi CommentR "withAuthorProfile"
         )

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
