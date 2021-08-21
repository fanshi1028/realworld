{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module VisitorAction (E (..), run) where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (Catch, catchError)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E)
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR (..))
import Domain.Util.Error (Impossible (Impossible), NotAuthorized, NotFound)
import Domain.Util.Field (Tag)
import Domain.Util.Representation (Transform (transform))
import qualified Relation.ManyToMany (E)
import qualified Relation.ToMany (E (GetRelated))
import qualified Storage.Map (E (GetAll, GetById))
import qualified Storage.Set (E (GetAll))

data E (m :: Type -> Type) a where
  GetProfile :: UserR "id" -> E m (UserR "profile")
  GetArticle :: ArticleR "id" -> E m (ArticleR "withAuthorProfile")
  ListArticles :: E m [ArticleR "withAuthorProfile"]
  GetTags :: E m [Tag]
  GetComments :: ArticleR "id" -> E m [CommentR "withAuthorProfile"]

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Member (Storage.Map.E UserR) sig,
    Member (Storage.Map.E ArticleR) sig,
    Member (Storage.Map.E CommentR) sig,
    Member (Storage.Set.E Tag) sig,
    Member (Relation.ManyToMany.E (ArticleR "id") "taggedBy" Tag) sig,
    Member (Relation.ManyToMany.E (UserR "id") "favorite" (ArticleR "id")) sig,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Member (Relation.ToMany.E (ArticleR "id") "has" (CommentR "id")) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Throw Impossible) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Catch (NotAuthorized UserR)) sig,
    Member (Catch (NotFound (CommentR "id"))) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      GetProfile uid -> send (Storage.Map.GetById @UserR uid) >>= transform
      GetArticle aid -> send (Storage.Map.GetById @ArticleR aid) >>= transform
      ListArticles -> runNonDetA @[] $ send (Storage.Map.GetAll @ArticleR) >>= oneOf >>= transform
      GetComments aid -> do
        void $ send (Storage.Map.GetById @ArticleR aid)
        runNonDetA @[] $
          send (Relation.ToMany.GetRelated @_ @"has" @(CommentR "id") aid)
            >>= oneOf
            >>= flip catchError (const @_ @(NotFound (CommentR "id")) $ throwError $ Impossible "comment id not found")
              . send
              . Storage.Map.GetById
            >>= transform
      GetTags -> send $ Storage.Set.GetAll @Tag
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
