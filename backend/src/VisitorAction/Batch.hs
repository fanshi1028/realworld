{-# LANGUAGE DataKinds #-}

-- |
module VisitorAction.Batch where

import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.Util.Field (Tag)

data E f (m :: Type -> Type) a where
  ListArticles :: E f m (f (ArticleR "withAuthorProfile"))
  GetTags :: E f m (f Tag)
  GetComments :: ArticleR "id" -> E f m (f (CommentR "withAuthorProfile"))
