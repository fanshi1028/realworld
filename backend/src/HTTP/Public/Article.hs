{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.Article (ArticleApi, articleServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (QP, ReadApi, ReadManyApi)
import Servant (Capture, ServerT, type (:<|>) ((:<|>)), type (:>))
import VisitorAction (E (GetArticle, GetComments, ListArticles))

type ArticleApi =
  QP "tag" :> QP "author" :> QP "favorited" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleR "withAuthorProfile"
    :<|> Capture "slug" (ArticleR "id")
      :> ( ReadApi ArticleR "withAuthorProfile"
             :<|> "comments" :> ReadManyApi CommentR "withAuthorProfile"
         )

-- FIXME
articleServer :: (Algebra sig m, Member VisitorAction.E sig) => ServerT ArticleApi m
articleServer =
  (\_ _ _ _ _ -> Out <$> send ListArticles)
    :<|> ( \aid ->
             Out <$> send (VisitorAction.GetArticle aid)
               :<|> (Out <$> send (GetComments aid))
         )
