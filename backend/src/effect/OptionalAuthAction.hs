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
import GHC.Records (getField)
import InMem.Relation
  ( ManyToManyRelationE,
    ToMany (..),
    ToManyRelationE,
    getRelatedLeftManyToMany,
    getRelatedRightManyToMany,
    isRelatedManyToMany,
  )
import InMem.Storage (MapInMemE, getAllMapInMem, getByIdMapInMem)
import InMem.Storage.Map (ContentOf (..), HasStorage (IdOf), IdNotFound, toUserId)
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
  ( MapInMemE 'User sig,
    MapInMemE 'Article sig,
    MapInMemE 'Comment sig,
    ManyToManyRelationE "ArticleTaggedByTag" sig,
    ManyToManyRelationE "UserFavoriteArticle" sig,
    ManyToManyRelationE "UserFollowUser" sig,
    ToManyRelationE "ArticleHasComment" sig,
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
        UserProfile . transform
          <$> getByIdMapInMem uid
          <*> ( R.ask >>= \case
                  Just (UserAuthWithToken (toUserId -> authId) _) -> do
                    _ <- getByIdMapInMem authId
                    isRelatedManyToMany @"UserFollowUser" authId uid
                  Nothing -> pure False
              )
      GetArticle aid -> do
        a <- getByIdMapInMem aid
        ArticleWithAuthorProfile a
          <$> getRelatedLeftManyToMany @"ArticleTaggedByTag" aid
          <*> pure False
          <*> (genericLength <$> getRelatedRightManyToMany @"UserFavoriteArticle" aid)
          <*> send (GetProfile $ getField @"author" a)
      ListArticles ->
        runNonDetA @[] $ do
          (aid, a) <- getAllMapInMem @'Article >>= oneOf
          ArticleWithAuthorProfile a
            <$> getRelatedLeftManyToMany @"ArticleTaggedByTag" aid
            <*> pure False
            <*> (genericLength <$> getRelatedRightManyToMany @"UserFavoriteArticle" aid)
            <*> send (GetProfile $ getField @"author" a)
      GetComments aid -> do
        _ <- getByIdMapInMem aid
        runNonDetA @[] $ do
          cid <- getRelatedToMany @"ArticleHasComment" aid >>= oneOf
          flip (catchError @(IdNotFound 'Comment)) (const $ throwError @Text "impossible: comment id not found") $ do
            CommentContent {..} <- getByIdMapInMem cid
            CommentWithAuthorProfile cid createdAt updatedAt body <$> send (GetProfile author)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
