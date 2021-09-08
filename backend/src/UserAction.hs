{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect and Carrier of users' action
--
-- @since 0.1.0.0
module UserAction where

import qualified Authentication (E)
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (Catch)
import Control.Effect.Error (catchError)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E (GetCurrent))
import Data.Generic.HKD (Build (build), Construct (construct), deconstruct)
import Data.Generics.Product (getField)
import Data.Password.Argon2 (hashPassword)
import qualified Data.Semigroup as SG
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists, Impossible (Impossible), NotAuthorized (NotAuthorized), NotFound (NotFound), ValidationErr)
import Domain.Util.Field (Tag, Time)
import Domain.Util.Representation (Transform (transform))
import Domain.Util.Update (WithUpdate, applyPatch)
import qualified GenUUID (E (Generate))
import qualified Relation.ManyToMany (E (GetRelatedLeft, GetRelatedRight, IsRelated, Relate, Unrelate, UnrelateByKeyRight))
import qualified Relation.ToMany (E (GetRelated, IsRelated, Relate, Unrelate, UnrelateByKey))
import qualified Storage.Map (E (DeleteById, GetById, Insert, UpdateById))
import qualified Token (E (CreateToken))

-- * Effect

-- | Actions that can only be carried out by __authenticated__ users.
--
-- @since 0.1.0.0
data E (m :: Type -> Type) a where
  -- | Get the info of the current authenticated user.
  --
  -- @since 0.1.0.0
  GetCurrentUser :: E m (UserR "authWithToken")
  -- | Update the profile of the current authenticated user.
  --
  -- @since 0.1.0.0
  UpdateUser :: UserR "update" -> E m (UserR "authWithToken")
  -- | Follow the user specified by the id.
  --
  -- @since 0.1.0.0
  FollowUser :: UserR "id" -> E m (UserR "profile")
  -- | Unfollow the user specified by the id.
  --
  -- @since 0.1.0.0
  UnfollowUser :: UserR "id" -> E m (UserR "profile")
  -- | Create the article specified by the id.
  --
  -- @since 0.1.0.0
  CreateArticle :: ArticleR "create" -> E m (ArticleR "withAuthorProfile")
  -- | Update the authenticated user's own article specified by the id.
  --
  -- @since 0.1.0.0
  UpdateArticle :: ArticleR "id" -> ArticleR "update" -> E m (ArticleR "withAuthorProfile")
  -- | Delete the authenticated user's own article specified by the id.
  --
  -- @since 0.1.0.0
  DeleteArticle :: ArticleR "id" -> E m ()
  -- | Leave a comment to the article specified by the id.
  --
  -- @since 0.1.0.0
  AddCommentToArticle :: ArticleR "id" -> CommentR "create" -> E m (CommentR "withAuthorProfile")
  -- | Delete the authenticated user's own comment specified by the id from the article.
  --
  -- @since 0.1.0.0
  DeleteComment :: ArticleR "id" -> CommentR "id" -> E m ()
  -- | Favorite the aritcle specified by the id.
  --
  -- @since 0.1.0.0
  FavoriteArticle :: ArticleR "id" -> E m (ArticleR "withAuthorProfile")
  -- | Unfavorite the aritcle specified by the id.
  --
  -- @since 0.1.0.0
  UnfavoriteArticle :: ArticleR "id" -> E m (ArticleR "withAuthorProfile")
  -- | Get articles feed recommendated for the authenticated user.
  --
  -- @since 0.1.0.0
  FeedArticles :: E m [ArticleR "withAuthorProfile"]

-- * Carrirer

-- | @since 0.1.0.0
newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.2.0.0
instance
  ( Member (Token.E UserR) sig,
    Member (Storage.Map.E UserR) sig,
    Member (Storage.Map.E ArticleR) sig,
    Member (Storage.Map.E CommentR) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotFound (UserR "id"))) sig,
    Member (Authentication.E UserR) sig,
    Member (Throw (AlreadyExists (ArticleR "id"))) sig,
    Member (Throw (NotFound (ArticleR "id"))) sig,
    Member (Catch (NotFound (ArticleR "id"))) sig,
    Member (Throw (NotFound (CommentR "id"))) sig,
    Member (Catch (NotFound (CommentR "id"))) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Throw Impossible) sig,
    Member (Current.E Time) sig,
    Member GenUUID.E sig,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Member (Relation.ManyToMany.E (UserR "id") "favorite" (ArticleR "id")) sig,
    Member (Relation.ManyToMany.E (ArticleR "id") "taggedBy" Tag) sig,
    Member (Relation.ToMany.E (ArticleR "id") "has" (CommentR "id")) sig,
    Member (Relation.ToMany.E (UserR "id") "create" (ArticleR "id")) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Catch (NotAuthorized UserR)) sig,
    Member (Lift IO) sig,
    Algebra sig m
  ) =>
  Algebra (UserAction.E :+: sig) (C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      authOut@(UserAuthWithToken auth _) <- send Current.GetCurrent
      let authUserId = transform @UserR @_ @"id" auth
      case action of
        GetCurrentUser -> pure authOut
        UpdateUser (UserUpdate update) ->
          send (Storage.Map.GetById authUserId) >>= \orig -> do
            update' <-
              construct $
                build @(WithUpdate (UserR "all"))
                  (pure $ getField @"email" update)
                  ( case getField @"password" update of
                      Just (SG.Last pwNew) -> do
                        hpw <- sendIO $ hashPassword pwNew
                        pure $ Just $ SG.Last hpw
                      Nothing -> pure Nothing
                  )
                  (pure $ getField @"username" update)
                  (pure $ getField @"bio" update)
                  (pure $ getField @"image" update)
            case construct $ deconstruct (deconstruct orig) <> update' of
              Nothing -> error "Impossible: Missing field when update"
              Just (construct -> SG.Last r) ->
                send (Storage.Map.UpdateById authUserId $ const r)
                  >>= \(transform -> newAuth) -> UserAuthWithToken newAuth <$> send (Token.CreateToken newAuth)
        FollowUser targetUserId -> do
          targetUser <- send $ Storage.Map.GetById targetUserId
          send $ Relation.ManyToMany.Relate @_ @_ @"follow" authUserId targetUserId
          pure $ UserProfile (transform targetUser) True
        UnfollowUser targetUserId -> do
          targetUser <- send $ Storage.Map.GetById targetUserId
          send (Relation.ManyToMany.Unrelate @_ @_ @"follow" authUserId targetUserId)
            $> UserProfile (transform targetUser) False
        CreateArticle create@(ArticleCreate tt des bd ts) -> do
          let aid = transform create
          send $ Relation.ToMany.Relate @_ @(ArticleR "id") @"create" authUserId aid
          t <- send $ Current.GetCurrent @Time
          foldMapA (send . Relation.ManyToMany.Relate @(ArticleR "id") @_ @"taggedBy" (transform create)) ts
          let a = Article tt des bd t t $ transform auth
          send (Storage.Map.Insert a)
            -- FIXME: Follow his own article?
            $> ArticleWithAuthorProfile aid a [] False 0 (UserProfile auth True)
        UpdateArticle articleId update -> do
          a <-
            send (Storage.Map.GetById articleId)
              <&> applyPatch update
              >>= send . Storage.Map.UpdateById articleId . const
          ArticleWithAuthorProfile articleId a
            <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
            <*> send (Relation.ManyToMany.IsRelated @_ @_ @"favorite" authUserId articleId)
            <*> (fromIntegral . length <$> send (Relation.ManyToMany.GetRelatedRight @_ @(UserR "id") @"favorite" articleId))
            -- FIXME: Follow his own article?
            <*> pure (UserProfile auth True)
        DeleteArticle articleId ->
          send (Storage.Map.GetById articleId)
            <&> (== authUserId) . getField @"author"
            >>= bool
              (throwError $ NotAuthorized @UserR)
              ( do
                  send $ Storage.Map.DeleteById articleId
                  send $ Relation.ToMany.Unrelate @_ @_ @"create" authUserId articleId
                  send $ Relation.ToMany.UnrelateByKey @_ @"has" @(CommentR "id") articleId
                  send $ Relation.ManyToMany.UnrelateByKeyRight @_ @(UserR "id") @"favorite" articleId
              )
        AddCommentToArticle articleId (CommentCreate txt) -> do
          t <- send $ Current.GetCurrent @Time
          commentId <- CommentId <$> send GenUUID.Generate
          let a = Comment commentId t t txt (transform auth) articleId
          send $ Storage.Map.Insert a
          send $ Relation.ToMany.Relate @_ @_ @"has" articleId commentId
          pure $
            CommentWithAuthorProfile commentId t t txt $
              -- FIXME is th current user following himself??
              UserProfile auth True
        DeleteComment articleId commentId -> do
          void $ send $ Storage.Map.GetById articleId
          send (Relation.ToMany.IsRelated @_ @_ @"has" articleId commentId)
            >>= bool
              (throwError $ NotFound commentId)
              (send $ Storage.Map.GetById commentId)
            <&> (== authUserId) . getField @"author"
            >>= bool
              (throwError $ NotAuthorized @UserR)
              ( do
                  send $ Storage.Map.DeleteById commentId
                  send $ Relation.ToMany.Unrelate @_ @_ @"has" articleId commentId
              )
        FavoriteArticle articleId -> do
          a <- send $ Storage.Map.GetById articleId
          send $ Relation.ManyToMany.Relate @_ @_ @"favorite" authUserId articleId
          let authorId = getField @"author" a
          ArticleWithAuthorProfile articleId a
            <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
            <*> send (Relation.ManyToMany.IsRelated @_ @_ @"favorite" authUserId articleId)
            <*> (fromIntegral . length <$> send (Relation.ManyToMany.GetRelatedRight @_ @(UserR "id") @"favorite" articleId))
            <*> ( UserProfile . transform
                    <$> send (Storage.Map.GetById authorId)
                    <*> send (Relation.ManyToMany.IsRelated @_ @_ @"follow" authUserId authorId)
                )
        UnfavoriteArticle articleId -> do
          a <- send $ Storage.Map.GetById articleId
          send $ Relation.ManyToMany.Unrelate @_ @_ @"favorite" authUserId articleId
          let authorId = getField @"author" a
          ArticleWithAuthorProfile articleId a
            <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
            <*> send (Relation.ManyToMany.IsRelated @_ @_ @"favorite" authUserId articleId)
            <*> (fromIntegral . length <$> send (Relation.ManyToMany.GetRelatedRight @_ @(UserR "id") @"favorite" articleId))
            <*> ( UserProfile . transform
                    <$> send (Storage.Map.GetById authorId)
                    <*> send (Relation.ManyToMany.IsRelated @_ @_ @"follow" authUserId authorId)
                )
        -- FIXME feed order
        FeedArticles -> runNonDetA @[] $ do
          send (Relation.ManyToMany.GetRelatedLeft @_ @"follow" authUserId)
            >>= oneOf
            >>= send . Relation.ToMany.GetRelated @(UserR "id") @"create"
            >>= oneOf
            >>= \articleId ->
              flip catchError (const @_ @(NotFound (ArticleR "id")) $ throwError $ Impossible "article id not found") $
                do
                  a <- send $ Storage.Map.GetById @ArticleR articleId
                  let authorId = getField @"author" a
                  -- TODO: factor out logic for author profile
                  ArticleWithAuthorProfile articleId a
                    <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
                    <*> send (Relation.ManyToMany.IsRelated @_ @_ @"favorite" authUserId articleId)
                    <*> (fromIntegral . length <$> send (Relation.ManyToMany.GetRelatedRight @_ @(UserR "id") @"favorite" articleId))
                    <*> ( UserProfile . transform
                            <$> send (Storage.Map.GetById authorId)
                            <*> send (Relation.ManyToMany.IsRelated @_ @_ @"follow" authUserId authorId)
                        )
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
