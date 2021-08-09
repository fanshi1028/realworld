{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module VisitorAction where

import qualified Authentication as Auth (E (Login))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (Catch)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists, Impossible (Impossible), NotAuthorized (NotAuthorized), NotFound (NotFound))
import Domain.Util.Field (Email, Tag, Time, Username)
import Domain.Util.Representation (Transform (transform))
import GHC.Records (HasField (getField))
import qualified Relation.ManyToMany (E)
import qualified Relation.OneToMany
import qualified Relation.OneToOne (E (Relate))
import qualified Storage.Map (E (GetAll, GetById, Insert))
import qualified Storage.Set (E (GetAll))

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

instance
  ( Member (Auth.E UserR) sig,
    Member (Storage.Map.E UserR) sig,
    Member (Storage.Map.E ArticleR) sig,
    Member (Storage.Map.E CommentR) sig,
    Member (Storage.Set.E Tag) sig,
    Member (Relation.OneToOne.E Email "of" (UserR "id")) sig,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Member (Relation.OneToMany.E (ArticleR "id") "has" (CommentR "id")) sig,
    Member (Throw Impossible) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Throw (AlreadyExists Username)) sig,
    Member (Throw (NotFound (UserR "id"))) sig,
    Member (Throw (NotFound (ArticleR "id"))) sig,
    Member (Current.E Time) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Catch (NotAuthorized UserR)) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      Register user -> do
        a <- transform user
        send $ Storage.Map.Insert @UserR a
        transform user >>= send . Relation.OneToOne.Relate @_ @(UserR "id") @"of" (getField @"email" user)
        transform a
      Login user -> do
        send (Auth.Login user)
          >>= send . Storage.Map.GetById @UserR
          -- TODO FIXME throw in Storage get
          >>= maybe (throwError $ NotAuthorized @UserR) transform
      GetProfile uid ->
        send (Storage.Map.GetById @UserR uid)
          >>= maybe (throwError $ NotFound uid) transform
      GetAritcle aid ->
        send (Storage.Map.GetById @ArticleR aid)
          >>= maybe (throwError $ NotFound aid) transform
      ListArticles -> runNonDetA @[] $ send (Storage.Map.GetAll @ArticleR) >>= oneOf >>= transform
      GetComments aid ->
        send (Storage.Map.GetById @ArticleR aid)
          >>= maybe
            (throwError $ NotFound aid)
            ( const $
                runNonDetA @[] $
                  send (Relation.OneToMany.GetRelated @_ @"has" @(CommentR "id") aid)
                    >>= oneOf
                    >>= send . Storage.Map.GetById
                    >>= maybe (throwError $ Impossible "comment id not found") transform
            )
      GetTags -> send $ Storage.Set.GetAll @Tag
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
