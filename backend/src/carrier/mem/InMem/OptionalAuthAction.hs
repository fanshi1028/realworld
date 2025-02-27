{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier of optionally authed action in memory
--
-- @since 0.3.0.0
module InMem.OptionalAuthAction where

import Authentication (AuthenticationE (GetCurrentAuth))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Data.Authentication.HasAuth (AuthOf (..))
import Data.Domain (Domain (Article, Comment, User))
import Data.Domain.Article (ArticleWithAuthorProfile (ArticleWithAuthorProfile))
import Data.Domain.Transform (transform)
import Data.Domain.User (UserProfile (UserProfile))
import Data.Storage.Map (IdNotFound)
import Data.Storage.Map.HasStorage (ContentOf (..), toUserId)
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
        let mkArticleOutput isFav isFollow =
              ArticleWithAuthorProfile a
                <$> getRelatedLeftManyToMany @ArticleTaggedByTag aid
                ?? isFav
                <*> (genericLength <$> getRelatedRightManyToMany @UserFavoriteArticle aid)
                <*> (UserProfile <$> getProfile uid ?? isFollow)
        authMaybe
          ( \authId ->
              join $
                mkArticleOutput
                  <$> isRelatedManyToMany @UserFollowUser authId uid
                  <*> isRelatedManyToMany @UserFavoriteArticle authId aid
          )
          (mkArticleOutput False False)
  alg hdl (R other) ctx = OptionalAuthActionInMemC $ alg (runOptionalAuthActionInMem . hdl) other ctx
  {-# INLINE alg #-}
