{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module UserAction where

import qualified Authentication (E)
import qualified Authentication.Token (E (CreateToken))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (Catch)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E (GetCurrent))
import Data.Generics.Product (getField)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists, Impossible (Impossible), NotAuthorized (NotAuthorized), NotFound (NotFound), ValidationErr)
import Domain.Util.Field (Time)
import Domain.Util.Representation (Transform (transform), applyPatch)
import qualified GenUUID (E)
import qualified Relation.ManyToMany (E (GetRelatedLeft, Relate, Unrelate, UnrelateByKeyRight))
import qualified Relation.OneToMany (E (GetRelated, Relate, Unrelate, UnrelateByKey))
import qualified Storage.Map (E (DeleteById, GetById, Insert, UpdateById))
import qualified Validation as V (Validation (Failure, Success))

data E (m :: Type -> Type) a where
  GetCurrentUser :: E m (UserR "authWithToken")
  UpdateUser :: UserR "update" -> E m (UserR "authWithToken")
  FollowUser :: UserR "id" -> E m (UserR "profile")
  UnfollowUser :: UserR "id" -> E m (UserR "profile")
  CreateArticle :: ArticleR "create" -> E m (ArticleR "withAuthorProfile")
  UpdateArticle :: ArticleR "id" -> ArticleR "update" -> E m (ArticleR "withAuthorProfile")
  DeleteArticle :: ArticleR "id" -> E m ()
  AddCommentToArticle :: ArticleR "id" -> CommentR "create" -> E m (CommentR "withAuthorProfile")
  DeleteComment :: ArticleR "id" -> CommentR "id" -> E m ()
  FavoriteArticle :: ArticleR "id" -> E m (ArticleR "withAuthorProfile")
  UnfavoriteArticle :: ArticleR "id" -> E m (ArticleR "withAuthorProfile")
  FeedArticles :: E m [ArticleR "withAuthorProfile"]
  GetCommentsFromArticle :: ArticleR "id" -> E m [CommentR "withAuthorProfile"]

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Member (Authentication.Token.E UserR) sig,
    Member (Storage.Map.E UserR) sig,
    Member (Storage.Map.E ArticleR) sig,
    Member (Storage.Map.E CommentR) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotFound (UserR "id"))) sig,
    Member (Authentication.E UserR) sig,
    Member (Throw (AlreadyExists (ArticleR "id"))) sig,
    Member (Throw (NotFound (ArticleR "id"))) sig,
    Member (Throw (NotFound (CommentR "id"))) sig,
    Member (Throw Impossible) sig,
    Member (Current.E Time) sig,
    Member GenUUID.E sig,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Member (Relation.ManyToMany.E (UserR "id") "favorite" (ArticleR "id")) sig,
    Member (Relation.OneToMany.E (ArticleR "id") "has" (CommentR "id")) sig,
    Member (Relation.OneToMany.E (UserR "id") "create" (ArticleR "id")) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Catch (NotAuthorized UserR)) sig,
    Algebra sig m
  ) =>
  Algebra (UserAction.E :+: sig) (C m)
  where
  alg hdl sig ctx = do
    authOut@(UserAuthWithToken auth _) <- send $ Current.GetCurrent @(UserR "authWithToken")
    authUserId <- transform @UserR @"auth" @"id" auth
    case sig of
      (L action) ->
        (<$ ctx) <$> case action of
          GetCurrentUser -> pure authOut
          UpdateUser update ->
            send (Storage.Map.GetById authUserId) >>= \case
              Nothing -> throwError $ NotFound authUserId
              Just (applyPatch update -> user) -> case user of
                V.Failure err -> throwError err
                V.Success new ->
                  send (Storage.Map.UpdateById authUserId $ const new)
                    >>= transform
                    >>= \newAuth -> UserAuthWithToken newAuth <$> send (Authentication.Token.CreateToken newAuth)
          FollowUser targetUserId ->
            send (Storage.Map.GetById targetUserId) >>= \case
              Nothing -> throwError $ NotFound targetUserId
              Just targetUser -> do
                send $ Relation.ManyToMany.Relate @_ @_ @"follow" authUserId targetUserId
                UserProfile <$> transform targetUser <*> pure True
          UnfollowUser targetUserId ->
            send (Storage.Map.GetById targetUserId) >>= \case
              Nothing -> throwError $ NotFound targetUserId
              Just targetUser -> do
                send $ Relation.ManyToMany.Unrelate @_ @_ @"follow" authUserId targetUserId
                UserProfile <$> transform targetUser <*> pure False
          CreateArticle create -> do
            transform create >>= send . Relation.OneToMany.Relate @_ @(ArticleR "id") @"create" authUserId
            a <- transform create
            send (Storage.Map.Insert a) >> transform a
          UpdateArticle articleId update ->
            send (Storage.Map.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just (applyPatch update -> article) -> case article of
                V.Failure err -> throwError err
                V.Success new -> send (Storage.Map.UpdateById articleId $ const new) >>= transform @_ @_ @"withAuthorProfile"
          DeleteArticle articleId ->
            send (Storage.Map.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just _ -> do
                send $ Storage.Map.DeleteById articleId
                send $ Relation.OneToMany.Unrelate @_ @_ @"create" authUserId articleId
                send $ Relation.OneToMany.UnrelateByKey @_ @"has" @(CommentR "id") articleId
                send $ Relation.ManyToMany.UnrelateByKeyRight @_ @(UserR "id") @"favorite" articleId
          AddCommentToArticle articleId cc@(CommentCreate comment) ->
            send (Storage.Map.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just article -> do
                let authorId = getField @"author" article
                send (Storage.Map.GetById authorId) >>= \case
                  Nothing -> throwError $ NotFound authorId
                  Just _ -> do
                    commentId <- transform cc
                    time <- send $ Current.GetCurrent @Time
                    send $ Storage.Map.Insert $ Comment commentId time time comment authorId articleId
                    send $ Relation.OneToMany.Relate @_ @_ @"has" articleId commentId
                    CommentWithAuthorProfile commentId time time comment <$> transform auth
          DeleteComment articleId commentId ->
            send (Storage.Map.GetById commentId) >>= \case
              Nothing -> throwError $ NotFound commentId
              Just _ -> do
                send $ Storage.Map.DeleteById commentId
                send $ Relation.OneToMany.Unrelate @_ @_ @"has" articleId commentId
          FavoriteArticle articleId ->
            send (Storage.Map.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just a -> do
                send $ Relation.ManyToMany.Relate @_ @_ @"favorite" authUserId articleId
                transform a
          UnfavoriteArticle articleId ->
            send (Storage.Map.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just a -> do
                send $ Relation.ManyToMany.Unrelate @_ @_ @"favorite" authUserId articleId
                transform a
          -- FIXME feed order
          FeedArticles -> runNonDetA @[] $ do
            send (Relation.ManyToMany.GetRelatedLeft @_ @"follow" authUserId)
              >>= oneOf
              >>= send . Relation.OneToMany.GetRelated @(UserR "id") @"create"
              >>= oneOf
              >>= send . Storage.Map.GetById @ArticleR
              >>= maybe (throwError $ Impossible "article id not found") transform
          GetCommentsFromArticle articleId ->
            send (Storage.Map.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just _ -> runNonDetA @[] $ do
                send (Relation.OneToMany.GetRelated @_ @"has" articleId)
                  >>= oneOf
                  >>= send . Storage.Map.GetById @CommentR
                  >>= maybe (throwError $ Impossible "comment id not found") transform
      R other -> C $ alg (run . hdl) other ctx
