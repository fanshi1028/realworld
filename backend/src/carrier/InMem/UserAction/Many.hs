{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier of user's action (many)
--
-- @since 0.3.0.0
module InMem.UserAction.Many where

import Authentication.HasAuth (AuthOf (..), NotLogin)
import Control.Algebra (Algebra, alg, send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetM)
import Control.Effect.Error (Catch, Throw, catchError, throwError)
import Control.Effect.NonDet (oneOf)
import qualified Control.Effect.Reader as R (Reader)
import Control.Effect.Sum (Member)
import Domain (Domain (Article, User))
import Domain.Article (ArticleR (ArticleWithAuthorProfile))
import Domain.User (UserR (UserAuthWithToken))
import GHC.Records (getField)
import InMem.Relation (ArticleTaggedByTag, ManyToMany (getRelatedLeftManyToMany), ManyToManyRelationE, ToMany (getRelatedToMany), ToManyRelationE, UserCreateArticle, UserFavoriteArticle, UserFollowUser, getRelatedRightManyToMany, isRelatedManyToMany)
import InMem.Storage (MapInMemE, getByIdMapInMem)
import OptionalAuthAction (OptionalAuthActionE (GetProfile))
import Storage.Map (ContentOf (..), IdNotFound, toUserId)
import UserAction (UserActionE (GetCurrentUser))
import UserAction.Many (UserActionManyE (FeedArticles))
import Util.Sort (getSorted)

-- | @since 0.3.0.0
newtype UserActionManyC (f :: Type -> Type) m a = UserActionManyC
  { -- | @since 0.3.0.0
    runUserActionManyInMem :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  ( MapInMemE 'Article sig,
    Member (Catch (IdNotFound 'Article)) sig,
    Member (Throw Text) sig,
    ManyToManyRelationE UserFollowUser sig,
    ManyToManyRelationE UserFavoriteArticle sig,
    ManyToManyRelationE ArticleTaggedByTag sig,
    ToManyRelationE UserCreateArticle sig,
    Member (Throw (NotLogin 'User)) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig,
    Member OptionalAuthActionE sig,
    Member UserActionE sig,
    Algebra sig m
  ) =>
  Algebra (UserActionManyE [] :+: sig) (UserActionManyC [] m)
  where
  alg _ (L FeedArticles) ctx =
    fmap ((<$ ctx) . getSorted) . runNonDetM pure $ do
      UserAuthWithToken (toUserId -> authUserId) _ <- send GetCurrentUser
      articleId <-
        getRelatedLeftManyToMany @UserFollowUser authUserId
          >>= oneOf
          >>= getRelatedToMany @UserCreateArticle
          >>= oneOf
      flip (catchError @(IdNotFound 'Article)) (const $ throwError @Text "impossible: article id not found") $ do
        a@(getField @"author" -> authorId) <- getByIdMapInMem articleId
        ArticleWithAuthorProfile a
          <$> getRelatedLeftManyToMany @ArticleTaggedByTag articleId
          <*> isRelatedManyToMany @UserFavoriteArticle authUserId articleId
          <*> (genericLength <$> getRelatedRightManyToMany @UserFavoriteArticle articleId)
          <*> send (GetProfile authorId)
  alg hdl (R other) ctx = UserActionManyC $ alg (runUserActionManyInMem . hdl) other ctx
  {-# INLINE alg #-}
