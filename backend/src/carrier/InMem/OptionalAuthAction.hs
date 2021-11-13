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

import Authentication.HasAuth (AuthOf (..))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleR (ArticleWithAuthorProfile))
import Domain.Transform (transform)
import Domain.User (UserR (UserAuthWithToken, UserProfile))
import GHC.Records (getField)
import InMem.Relation
  ( ManyToManyRelationE,
    ToManyRelationE,
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
    ManyToManyRelationE "ArticleTaggedByTag" sig,
    ManyToManyRelationE "UserFavoriteArticle" sig,
    ManyToManyRelationE "UserFollowUser" sig,
    ToManyRelationE "ArticleHasComment" sig,
    Member (Throw Text) sig,
    Member (Catch (IdNotFound 'Comment)) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig,
    Algebra sig m
  ) =>
  Algebra (OptionalAuthActionE :+: sig) (OptionalAuthActionInMemC m)
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
          <*> ( R.ask >>= \case
                  Just (UserAuthWithToken (toUserId -> authId) _) -> do
                    _ <- getByIdMapInMem authId
                    isRelatedManyToMany @"UserFavoriteArticle" authId aid
                  Nothing -> pure False
              )
          <*> (genericLength <$> getRelatedRightManyToMany @"UserFavoriteArticle" aid)
          <*> send (GetProfile $ getField @"author" a)
  alg hdl (R other) ctx = OptionalAuthActionInMemC $ alg (runOptionalAuthActionInMem . hdl) other ctx
  {-# INLINE alg #-}
