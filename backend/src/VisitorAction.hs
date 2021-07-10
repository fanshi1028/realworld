{-# LANGUAGE DataKinds #-}

-- |
module VisitorAction where

import Domain.Article (ArticleR)
import Domain.User (UserR)

data E (m :: Type -> Type) a where
  Register :: UserR "create" -> E m (UserR "auth")
  Login :: UserR "login" -> E m (UserR "auth")
  GetProfile :: UserR "id" -> E m (UserR "profile")
  GetAritcle :: ArticleR "id" -> E m (ArticleR "withAuthorProfile")
