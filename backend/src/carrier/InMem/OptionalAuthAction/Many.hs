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
-- Carrier of optionally authed action (many)
--
-- @since 0.3.0.0
module InMem.OptionalAuthAction.Many where

import Control.Algebra (Algebra, alg, send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (catchError)
import Control.Effect.Error (Catch, Throw, throwError)
import Control.Effect.NonDet (oneOf)
import qualified Control.Effect.Reader as R (Reader)
import Control.Effect.Sum (Member)
import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleR (ArticleWithAuthorProfile))
import Domain.Comment (CommentR (CommentWithAuthorProfile))
import Domain.User (UserR)
import GHC.Records (getField)
import InMem.Relation (ManyToManyRelationE, ToMany (getRelatedToMany), ToManyRelationE, getRelatedLeftManyToMany, getRelatedRightManyToMany)
import InMem.Storage (MapInMemE, getAllMapInMem, getByIdMapInMem)
import OptionalAuthAction (OptionalAuthActionE (GetProfile))
import OptionalAuthAction.Many (OptionalAuthActionManyE (GetComments, ListArticles))
import Storage.Map (ContentOf (..), IdNotFound)
import Prelude hiding (id)

-- | @since 0.3.0.0
newtype OptionalAuthActionManyC (f :: Type -> Type) m a = OptionalAuthActionManyC
  { -- | @since 0.3.0.0
    runOptionalAuthActionManyInMem :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  ( MapInMemE 'User sig,
    MapInMemE 'Article sig,
    MapInMemE 'Comment sig,
    ManyToManyRelationE "ArticleTaggedByTag" sig,
    ManyToManyRelationE "UserFavoriteArticle" sig,
    ToManyRelationE "ArticleHasComment" sig,
    Member (Throw Text) sig,
    Member (Catch (IdNotFound 'Comment)) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig,
    Member OptionalAuthActionE sig,
    Alternative f,
    Algebra sig m
  ) =>
  Algebra (OptionalAuthActionManyE f :+: sig) (OptionalAuthActionManyC f m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      ListArticles mTag mAuthor mFav ->
        runNonDetA @[] $ do
          let mkPred :: forall a b. (a -> b -> Bool) -> Maybe a -> Predicate b
              mkPred pp = maybe mempty (Predicate . pp)
              predGuard :: forall m a. (Alternative m, Monad m) => Predicate a -> m a -> m a
              predGuard (getPredicate -> p) ma = do
                a <- ma
                guard $ p a
                pure a
          (aid, a) <- getAllMapInMem @'Article >>= oneOf
          favBy <- getRelatedRightManyToMany @"UserFavoriteArticle" aid
          follow <-
            R.ask >>= \case
              Just (UserAuthWithToken (toUserId -> authId) _) -> do
                _ <- getByIdMapInMem authId
                pure $ authId `elem` favBy
              Nothing -> pure False
          case mFav of
            Nothing -> pure ()
            Just (elem . UserId -> check) -> guard $ check favBy
          tags <- predGuard (mkPred elem mTag) (getRelatedLeftManyToMany @"ArticleTaggedByTag" aid)
          ArticleWithAuthorProfile a tags follow (genericLength favBy)
            <$> (predGuard (mkPred ((==) . UserId) mAuthor) (pure $ getField @"author" a) >>= send . GetProfile)
      GetComments aid -> do
        _ <- getByIdMapInMem aid
        runNonDetA @[] $ do
          cid <- getRelatedToMany @"ArticleHasComment" aid >>= oneOf
          flip (catchError @(IdNotFound 'Comment)) (const $ throwError @Text "impossible: comment id not found") $ do
            CommentContent {..} <- getByIdMapInMem cid
            CommentWithAuthorProfile cid createdAt updatedAt body <$> send (GetProfile author)
  alg hdl (R other) ctx = OptionalAuthActionManyC $ alg (runOptionalAuthActionManyInMem . hdl) other ctx
  {-# INLINE alg #-}