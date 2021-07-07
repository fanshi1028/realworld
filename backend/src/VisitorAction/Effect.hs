{-# LANGUAGE DataKinds #-}

-- |
module VisitorAction.Effect where

import Domain.Article (ArticleR)
import Domain.User (UserR)

data VisitorAction (m :: Type -> Type) a where
  Register :: UserR "register" -> VisitorAction m (UserR "auth")
  GetProfile :: UserR "id" -> VisitorAction m (UserR "profile")
  GetAritcle :: ArticleR "id" -> VisitorAction m (ArticleR "withAuthorProfile")
