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
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E (GetCurrent))
import Data.Generics.Product (getField)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists, Impossible (Impossible), NotAuthorized (NotAuthorized), NotFound (NotFound), ValidationErr)
import Domain.Util.Field (Tag, Time)
import Domain.Util.Representation (Transform (transform))
import Domain.Util.Update (applyPatch)
import qualified GenUUID (E)
import qualified Relation.ManyToMany (E (GetRelatedLeft, Relate, Unrelate, UnrelateByKeyRight))
import qualified Relation.ToMany (E (GetRelated, IsRelated, Relate, Unrelate, UnrelateByKey))
import qualified Storage.Map (E (DeleteById, GetById, Insert, UpdateById))
import qualified Token (E (CreateToken))
import Validation (validation)

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

-- | @since 0.1.0.0
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
    Algebra sig m
  ) =>
  Algebra (UserAction.E :+: sig) (C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      authOut@(UserAuthWithToken auth _) <- send Current.GetCurrent
      authUserId <- transform @UserR @_ @"id" auth
      case action of
        GetCurrentUser -> pure authOut
        UpdateUser update ->
          send (Storage.Map.GetById authUserId)
            <&> applyPatch update
              >>= validation throwError (send . Storage.Map.UpdateById authUserId . const)
              >>= transform
              >>= \newAuth -> UserAuthWithToken newAuth <$> send (Token.CreateToken newAuth)
        FollowUser targetUserId -> do
          targetUser <- send $ Storage.Map.GetById targetUserId
          send $ Relation.ManyToMany.Relate @_ @_ @"follow" authUserId targetUserId
          UserProfile <$> transform targetUser <*> pure True
        UnfollowUser targetUserId -> do
          targetUser <- send $ Storage.Map.GetById targetUserId
          send $ Relation.ManyToMany.Unrelate @_ @_ @"follow" authUserId targetUserId
          UserProfile <$> transform targetUser <*> pure False
        CreateArticle create -> do
          transform create >>= send . Relation.ToMany.Relate @_ @(ArticleR "id") @"create" authUserId
          a <- transform create
          send (Storage.Map.Insert a) >> transform a
        UpdateArticle articleId update ->
          send (Storage.Map.GetById articleId)
            <&> applyPatch update
            >>= validation throwError (send . Storage.Map.UpdateById articleId . const)
            >>= transform @_ @_ @"withAuthorProfile"
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
        AddCommentToArticle articleId create -> do
          a <- usingReaderT articleId $ transform create
          send $ Storage.Map.Insert a
          commentId <- transform a
          send $ Relation.ToMany.Relate @_ @_ @"has" articleId commentId
          send (Storage.Map.GetById commentId) >>= transform
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
          transform a
        UnfavoriteArticle articleId -> do
          a <- send $ Storage.Map.GetById articleId
          send $ Relation.ManyToMany.Unrelate @_ @_ @"favorite" authUserId articleId
          transform a
        -- FIXME feed order
        FeedArticles -> runNonDetA @[] $ do
          send (Relation.ManyToMany.GetRelatedLeft @_ @"follow" authUserId)
            >>= oneOf
            >>= send . Relation.ToMany.GetRelated @(UserR "id") @"create"
            >>= oneOf
            >>= flip catchError (const @_ @(NotFound (ArticleR "id")) $ throwError $ Impossible "article id not found")
              . send
              . Storage.Map.GetById @ArticleR
            >>= transform
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
