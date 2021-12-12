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
module InMem.OptionalAuthAction where

import Authentication (AuthenticationE (GetCurrentAuth))
import Authentication.HasAuth (AuthOf (..))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleR (ArticleWithAuthorProfile))
import Domain.Transform (transform)
import Domain.User (UserR (UserProfile))
import GHC.Records (getField)
import InMem.Relation
  ( ArticleHasComment,
    ArticleTaggedByTag,
    ManyToManyRelationE,
    ToManyRelationE,
    UserFavoriteArticle,
    UserFollowUser,
    getRelatedLeftManyToMany,
    getRelatedRightManyToMany,
    isRelatedManyToMany,
  )
import InMem.Storage (MapInMemE, getByIdMapInMem)
import OptionalAuthAction (OptionalAuthActionE (GetArticle, GetProfile))
import Storage.Map (ContentOf (..), IdNotFound, toUserId)
import Prelude hiding (id)

-- | @since 0.3.0.0
newtype OptionalAuthActionInMemC m a = OptionalAuthActionInMemC
  { -- | @since 0.3.0.0
    runOptionalAuthActionInMem :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  ( MapInMemE 'User sig,
    MapInMemE 'Article sig,
    MapInMemE 'Comment sig,
    ManyToManyRelationE ArticleTaggedByTag sig,
    ManyToManyRelationE UserFavoriteArticle sig,
    ManyToManyRelationE UserFollowUser sig,
    ToManyRelationE ArticleHasComment sig,
    Member (Throw Text) sig,
    Member (Catch (IdNotFound 'Comment)) sig,
    Member (AuthenticationE 'User) sig,
    Algebra sig m
  ) =>
  Algebra (OptionalAuthActionE :+: sig) (OptionalAuthActionInMemC m)
  where
  alg _ (L action) ctx = do
    let authMaybe f no =
          send (GetCurrentAuth @'User) >>= \case
            Just (toUserId -> authId) -> do
              _ <- getByIdMapInMem authId
              f authId
            Nothing -> no
        getProfile uid = transform <$> getByIdMapInMem uid
    (<$ ctx) <$> case action of
      GetProfile uid ->
        UserProfile
          <$> getProfile uid <*> authMaybe (\authId -> isRelatedManyToMany @UserFollowUser authId uid) (pure False)
      GetArticle aid -> do
        a@(getField @"author" -> uid) <- getByIdMapInMem aid
        let mkArticleOutput getIsFav getIsFollow =
              ArticleWithAuthorProfile a
                <$> getRelatedLeftManyToMany @ArticleTaggedByTag aid
                <*> getIsFav
                <*> (genericLength <$> getRelatedRightManyToMany @UserFavoriteArticle aid)
                <*> (UserProfile <$> getProfile uid <*> getIsFollow)
        authMaybe
          ( \authId ->
              mkArticleOutput
                (isRelatedManyToMany @UserFollowUser authId uid)
                $ isRelatedManyToMany @UserFavoriteArticle authId aid
          )
          (mkArticleOutput (pure False) $ pure False)
  alg hdl (R other) ctx = OptionalAuthActionInMemC $ alg (runOptionalAuthActionInMem . hdl) other ctx
  {-# INLINE alg #-}
