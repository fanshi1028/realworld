{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module VisitorAction where

import qualified Authentication as Auth (E (Login))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR (..))
import Domain.Util.Error (NotAuthorized (NotAuthorized))
import Domain.Util.Field (Tag)
import Domain.Util.Representation (Transform (transform))
import qualified Storage (E (GetById))
import qualified Tag

data E (m :: Type -> Type) a where
  Register :: UserR "create" -> E m (UserR "auth")
  Login :: UserR "login" -> E m (UserR "auth")
  GetProfile :: UserR "id" -> E m (UserR "profile")
  GetAritcle :: ArticleR "id" -> E m (ArticleR "withAuthorProfile")
  ListArticles :: E m [ArticleR "withAuthorProfile"]
  GetTags :: E m [Tag]
  GetComments :: ArticleR "id" -> E m [CommentR "withAuthorProfile"]

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- FIXME
instance
  ( Member (Throw (NotAuthorized UserR)) sig,
    Member (Auth.E UserR) sig,
    Member (Storage.E UserR) sig,
    Member (Tag.E []) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg hdl (L (Register user)) ctx = pure $ undefined <$ ctx
  alg hdl (L (Login user)) ctx =
    send (Auth.Login user)
      >>= send . Storage.GetById @UserR
      >>= \case
        Nothing -> throwError $ NotAuthorized @UserR
        Just user' -> (<$ ctx) <$> transform user'
  alg hdl (L (GetProfile ur)) ctx = pure $ undefined <$ ctx
  alg hdl (L (GetAritcle ar)) ctx = pure $ undefined <$ ctx
  alg hdl (L ListArticles) ctx = pure $ undefined <$ ctx
  alg hdl (L (GetComments _)) ctx = pure $ undefined <$ ctx
  alg hdl (L GetTags) ctx = (<$ ctx) <$> send Tag.GetTags
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
