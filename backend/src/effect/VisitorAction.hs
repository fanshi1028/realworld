{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect and Carrier of visitors' action
--
-- @since 0.1.0.0
module VisitorAction where

import Article (ArticleR (ArticleWithAuthorProfile))
import Comment (CommentR (CommentWithAuthorProfile))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (Catch, catchError)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E)
import Field.Tag (Tag)
import GHC.Records (getField)
import qualified Relation.ManyToMany (E (GetRelatedLeft, GetRelatedRight))
import qualified Relation.ToMany (E (GetRelated))
import Storage.Map (ContentOf (..), HasStorage (IdOf), toArticleId)
import qualified Storage.Map (E (GetAll, GetById))
import qualified Storage.Set (E (GetAll))
import User (UserR (UserProfile))
import Util.Error (Impossible (Impossible), NotAuthorized, NotFound)
import Util.Representation (Transform (transform))

-- * Effect

-- | Actions that can be carried out by visitor(__unauthenticated__).
--
-- @since 0.1.0.0
data E (m :: Type -> Type) a where
  -- | Get the profile of the user specified by the id.
  --
  -- @since 0.1.0.0
  GetProfile :: IdOf "user" -> E m (UserR "profile")
  -- | Get the article specified by the id.
  --
  -- @since 0.1.0.0
  GetArticle :: IdOf "article" -> E m (ArticleR "withAuthorProfile")
  -- | Get all the articles.
  --
  -- @since 0.1.0.0
  ListArticles :: E m [ArticleR "withAuthorProfile"]
  -- | Get all the tags.
  --
  -- @since 0.1.0.0
  GetTags :: E m [Tag]
  -- | Get all the comments of the article specified by the id.
  --
  -- @since 0.1.0.0
  GetComments :: IdOf "article" -> E m [CommentR "withAuthorProfile"]

-- * Carrirer

-- | @since 0.1.0.0
newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( Member (Storage.Map.E "user") sig,
    Member (Storage.Map.E "article") sig,
    Member (Storage.Map.E "comment") sig,
    Member (Storage.Set.E Tag) sig,
    Member (Relation.ManyToMany.E (IdOf "article") "taggedBy" Tag) sig,
    Member (Relation.ManyToMany.E (IdOf "user") "favorite" (IdOf "article")) sig,
    Member (Relation.ManyToMany.E (IdOf "user") "follow" (IdOf "user")) sig,
    Member (Relation.ToMany.E (IdOf "article") "has" (IdOf "comment")) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Throw Impossible) sig,
    -- Member (Throw (NotAuthorized UserR)) sig,
    -- Member (Catch (NotAuthorized UserR)) sig,
    Member (Catch (NotFound (IdOf "comment"))) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      -- FIXME: options auth? following?
      GetProfile uid -> send (Storage.Map.GetById uid) <&> flip UserProfile False . transform
      GetArticle aid -> do
        a <- send $ Storage.Map.GetById aid
        let uid = getField @"author" a
        u <- transform <$> send (Storage.Map.GetById uid)
        ArticleWithAuthorProfile a
          <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag aid)
          <*> pure False
          <*> (genericLength <$> send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf "user") @"favorite" aid))
          <*> pure (UserProfile u False)
      ListArticles ->
        runNonDetA @[] $ do
          a <- send (Storage.Map.GetAll @"article") >>= oneOf
          let aid = toArticleId a
              uid = getField @"author" a
          u <- transform <$> send (Storage.Map.GetById uid)
          ArticleWithAuthorProfile a
            <$> send (Relation.ManyToMany.GetRelatedLeft @(IdOf "article") @"taggedBy" @Tag aid)
            <*> pure False
            <*> (genericLength <$> send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf "user") @"favorite" aid))
            <*> pure (UserProfile u False)
      GetComments aid -> do
        _ <- send $ Storage.Map.GetById aid
        runNonDetA @[] $ do
          cid <- send (Relation.ToMany.GetRelated @_ @"has" @(IdOf "comment") aid) >>= oneOf
          flip (catchError @(NotFound (IdOf "comment"))) (const $ throwError $ Impossible "comment id not found") $ do
            Comment {..} <- send $ Storage.Map.GetById cid
            auth <- transform <$> send (Storage.Map.GetById author)
            pure $ CommentWithAuthorProfile cid createdAt updatedAt body $ UserProfile auth False
      GetTags -> send $ Storage.Set.GetAll @Tag
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
