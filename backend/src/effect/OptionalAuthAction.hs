{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect and Carrier of optionally authed action
--
-- @since 0.3.0.0
module OptionalAuthAction where

import Authentication (AuthOf (..))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (Catch, catchError)
import Control.Effect.NonDet (oneOf)
import qualified Control.Effect.Reader as R
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleR (ArticleWithAuthorProfile))
import Domain.Comment (CommentR (CommentWithAuthorProfile))
import Domain.Transform (transform)
import Domain.User (UserR (UserAuthWithToken, UserProfile))
import Field.Tag (Tag)
import GHC.Records (getField)
import qualified Relation.ManyToMany (E (GetRelatedLeft, GetRelatedRight, IsRelated))
import qualified Relation.ToMany (E (GetRelated))
import Storage.Map (ContentOf (..), HasStorage (IdOf), IdNotFound, toArticleId, toUserId)
import qualified Storage.Map (E (GetAll, GetById))
import Prelude hiding (id)

-- * Effect

-- | @since 0.3.0.0
-- Optionally authed actions that can be carried out by visitors or users with different behaviours.
data E (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Get the profile of the user specified by the id.
  GetProfile :: IdOf 'User -> E m (UserR "profile")
  -- | @since 0.3.0.0
  -- Get the article specified by the id.
  GetArticle :: IdOf 'Article -> E m (ArticleR "withAuthorProfile")
  -- | @since 0.3.0.0
  -- Get all the articles.
  ListArticles :: E m [ArticleR "withAuthorProfile"]
  -- | @since 0.3.0.0
  -- Get all the comments of the article specified by the id.
  GetComments :: IdOf 'Article -> E m [CommentR "withAuthorProfile"]

-- * Carrirer

-- | @since 0.3.0.0
newtype C m a = C
  { -- | @since 0.3.0.0
    run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  ( Member (Storage.Map.E 'User) sig,
    Member (Storage.Map.E 'Article) sig,
    Member (Storage.Map.E 'Comment) sig,
    Member (Relation.ManyToMany.E (IdOf 'Article) "taggedBy" Tag) sig,
    Member (Relation.ManyToMany.E (IdOf 'User) "favorite" (IdOf 'Article)) sig,
    Member (Relation.ManyToMany.E (IdOf 'User) "follow" (IdOf 'User)) sig,
    Member (Relation.ToMany.E (IdOf 'Article) "has" (IdOf 'Comment)) sig,
    Member (Throw Text) sig,
    Member (Catch (IdNotFound 'Comment)) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      GetProfile uid ->
        UserProfile
          <$> (transform <$> send (Storage.Map.GetById uid))
          <*> ( R.ask >>= \case
                  Just (UserAuthWithToken (toUserId -> authId) _) -> do
                    _ <- send $ Storage.Map.GetById authId
                    send $ Relation.ManyToMany.IsRelated @_ @_ @"follow" authId uid
                  Nothing -> pure False
              )
      GetArticle aid -> do
        a <- send $ Storage.Map.GetById aid
        let uid = getField @"author" a
        u <- transform <$> send (Storage.Map.GetById uid)
        ArticleWithAuthorProfile a
          <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag aid)
          <*> pure False
          <*> (genericLength <$> send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf 'User) @"favorite" aid))
          <*> send (GetProfile u)
      ListArticles ->
        runNonDetA @[] $ do
          a <- send (Storage.Map.GetAll @'Article) >>= oneOf
          let aid = toArticleId a
              uid = getField @"author" a
          u <- transform <$> send (Storage.Map.GetById uid)
          ArticleWithAuthorProfile a
            <$> send (Relation.ManyToMany.GetRelatedLeft @(IdOf 'Article) @"taggedBy" @Tag aid)
            <*> pure False
            <*> (genericLength <$> send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf 'User) @"favorite" aid))
            <*> send (GetProfile u)
      GetComments aid -> do
        _ <- send $ Storage.Map.GetById aid
        runNonDetA @[] $ do
          cid <- send (Relation.ToMany.GetRelated @_ @"has" @(IdOf 'Comment) aid) >>= oneOf
          flip (catchError @(IdNotFound 'Comment)) (const $ throwError @Text "impossible: comment id not found") $ do
            CommentContent {..} <- send $ Storage.Map.GetById cid
            auth <- transform <$> send (Storage.Map.GetById author)
            CommentWithAuthorProfile cid createdAt updatedAt body <$> send (GetProfile auth)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
