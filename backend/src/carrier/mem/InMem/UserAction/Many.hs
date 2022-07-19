{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Authentication (AuthenticationE (GetCurrentAuth))
import Control.Algebra (Algebra, alg, send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetM)
import Control.Effect.Error (Catch, Throw, catchError, throwError)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import Data.Authentication.HasAuth (AuthOf (..))
import Data.Domain (Domain (Article, User))
import Data.Domain.Article (ArticleWithAuthorProfile (ArticleWithAuthorProfile))
import Data.Storage.Map (IdNotFound)
import Data.Storage.Map.HasStorage (ContentOf (..), toUserId)
import Data.Util.Sort (getSorted)
import GHC.Records (getField)
import InMem.Relation (ArticleTaggedByTag, ManyToMany (getRelatedLeftManyToMany), ManyToManyRelationE, ToMany (getRelatedToMany), ToManyRelationE, UserCreateArticle, UserFavoriteArticle, UserFollowUser, getRelatedRightManyToMany, isRelatedManyToMany)
import InMem.Storage (MapInMemE, getByIdMapInMem)
import OptionalAuthAction (OptionalAuthActionE (GetProfile))
import UserAction.Many (UserActionManyE (FeedArticles))

-- | @since 0.3.0.0
newtype UserActionManyInMemC (f :: Type -> Type) m a = UserActionManyInMemC
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
    Member (AuthenticationE 'User) sig,
    Member OptionalAuthActionE sig,
    Algebra sig m
  ) =>
  Algebra (UserActionManyE [] :+: sig) (UserActionManyInMemC [] m)
  where
  alg _ (L FeedArticles) ctx =
    fmap ((<$ ctx) . getSorted) . runNonDetM pure $ do
      authUserId <-
        send (GetCurrentAuth @'User)
          >>= maybe (error "impossible: not login would not get here") (pure . toUserId)
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
  alg hdl (R other) ctx = UserActionManyInMemC $ alg (runUserActionManyInMem . hdl) other ctx
  {-# INLINE alg #-}
