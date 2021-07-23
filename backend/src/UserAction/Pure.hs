{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module UserAction.Pure where

import qualified Authentication (E)
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Reader (Reader)
import qualified Control.Effect.Reader as R
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Control.Exception.Safe (MonadCatch, MonadThrow, catch)
import qualified CurrentTime (E (GetCurrentTime))
import Data.Generics.Internal.VL (over)
import Data.Generics.Product (HasField' (field'), getField)
import qualified Data.HashSet as HS (delete, insert)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..), bio, email, image, username)
import Domain.Util.Error (AlreadyExists, NotFound (NotFound), ValidationErr)
import Domain.Util.Representation (Transform (transform), applyPatch)
import qualified GenID (E (GenerateID))
import qualified Storage.STM as STM (E (DeleteById, GetById, Insert, UpdateById))
import qualified Transform (E (Transform))
import UserAction (E (AddCommentToArticle, CreateArticle, DeleteArticle, DeleteComment, FavoriteArticle, FollowUser, UnfavoriteArticle, UnfollowUser, UpdateArticle, UpdateUser))
import qualified Validation as V (Validation (Failure, Success))
import Prelude hiding (Reader, ask)

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance
  ( Member (Reader (UserR "auth")) sig,
    Member (STM.E UserR) sig,
    Member (STM.E ArticleR) sig,
    Member (GenID.E ArticleR) sig,
    Member (STM.E CommentR) sig,
    Member (GenID.E CommentR) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotFound (UserR "id"))) sig,
    Member (Authentication.E UserR) sig,
    Member (Throw (AlreadyExists (ArticleR "id"))) sig,
    Member (Throw (NotFound (ArticleR "id"))) sig,
    Member (Transform.E ArticleR "all" "withAuthorProfile") sig,
    Member (Transform.E ArticleR "create" "all") sig,
    Member CurrentTime.E sig,
    Algebra sig m,
    MonadIO m,
    MonadCatch m,
    MonadThrow m
  ) =>
  Algebra (UserAction.E :+: sig) (C m)
  where
  alg hdl sig ctx = do
    let (%~) = over
    authUserId <- R.asks (transform @UserR @"auth" @"id")
    case sig of
      (L (UpdateUser update)) -> do
        getById <- send STM.GetById <*> pure authUserId
        updateById <- send STM.UpdateById <*> pure authUserId
        let stm =
              getById >>= \case
                Nothing -> throwSTM $ NotFound authUserId
                Just (applyPatch update -> user) -> case user of
                  (V.Failure err) -> throwSTM err
                  (V.Success new) -> updateById $ const new
        (<$ ctx) . transform
          <$> ( liftIO (atomically stm)
                  `catch` throwError @(NotFound (UserR "id"))
                  `catch` throwError @ValidationErr
              )
      (L (FollowUser userId)) -> do
        getTargetUser <- send STM.GetById <*> pure userId
        updatCurrentUser <- send STM.UpdateById <*> pure authUserId
        updateTargetUser <- send STM.UpdateById <*> pure userId
        let stm =
              getTargetUser >>= \case
                Nothing -> throwSTM $ NotFound userId
                Just _ ->
                  updatCurrentUser (field' @"following" %~ HS.insert userId)
                    *> updateTargetUser (field' @"followBy" %~ HS.insert authUserId)
        User {..} <-
          liftIO (atomically stm)
            `catch` throwError @(NotFound (UserR "id"))
        pure $ UserProfile email username bio image True <$ ctx
      (L (UnfollowUser userId)) -> do
        getTargetUser <- send STM.GetById <*> pure userId
        updatCurrentUser <- send STM.UpdateById <*> pure authUserId
        updateTargetUser <- send STM.UpdateById <*> pure userId
        let stm =
              getTargetUser >>= \case
                Nothing -> throwSTM $ NotFound userId
                Just _ ->
                  updatCurrentUser (field' @"following" %~ HS.delete userId)
                    *> updateTargetUser (field' @"followBy" %~ HS.delete authUserId)
        User {..} <-
          liftIO (atomically stm)
            `catch` throwError @(NotFound (UserR "id"))
        pure $ UserProfile email username bio image False <$ ctx
      (L (CreateArticle create)) -> do
        stm <-
          send STM.Insert <*> send (GenID.GenerateID create) <*> send (Transform.Transform create)
        liftIO (atomically stm)
          `catch` throwError @(AlreadyExists (ArticleR "id"))
          >>= ((<$ ctx) <$>) . send . Transform.Transform @_ @_ @"withAuthorProfile"
      (L (UpdateArticle articleId update)) -> do
        getById <- send STM.GetById <*> pure articleId
        updateById <- send STM.UpdateById <*> pure articleId
        let stm =
              getById >>= \case
                Nothing -> throwSTM $ NotFound articleId
                Just (applyPatch update -> article) -> case article of
                  (V.Failure err) -> throwSTM err
                  (V.Success new) -> updateById $ const new
        liftIO (atomically stm)
          `catch` throwError @(NotFound (ArticleR "id"))
          `catch` throwError @ValidationErr
          >>= ((<$ ctx) <$>) . send . Transform.Transform @_ @_ @"withAuthorProfile"
      (L (DeleteArticle articleId)) -> do
        getById <- send STM.GetById <*> pure articleId
        deleteById <- send STM.DeleteById <*> pure articleId
        let stm =
              getById >>= \case
                Nothing -> throwSTM $ NotFound articleId
                Just _ -> deleteById
        (<$ ctx) <$> liftIO (atomically stm) `catch` throwError @(NotFound (ArticleR "id"))
      (L (AddCommentToArticle articleId cc@(CommentCreate comment))) -> do
        time <- send CurrentTime.GetCurrentTime
        getById <- send STM.GetById <*> pure articleId
        commentId <- send $ GenID.GenerateID cc
        insert <- send STM.Insert <*> pure commentId
        findAuthor <- send STM.GetById
        let stm =
              getById >>= \case
                Nothing -> throwSTM $ NotFound articleId
                Just article -> do
                  let authorId = getField @"author" article
                  findAuthor authorId >>= \case
                    Nothing -> throwSTM $ NotFound authorId
                    Just User {..} -> do
                      -- FIXME
                      let profile = UserProfile email username bio image undefined
                      insert (Comment commentId time time comment authorId)
                        >> pure (CommentWithAuthorProfile commentId time time comment profile <$ ctx)
        liftIO (atomically stm) `catch` throwError @(NotFound (ArticleR "id"))
      -- FIXME
      (L (DeleteComment articleId commentId)) -> do
        getComment <- send STM.GetById <*> pure commentId
        deleteComment <- send STM.DeleteById <*> pure commentId
        let stm =
              getComment >>= \case
                Nothing -> throwSTM $ NotFound commentId
                (Just cm) -> do
                  -- FIXME
                  pure undefined
        -- FIXME
        pure undefined
      -- FIXME
      (L (FavoriteArticle articleId)) -> undefined
      -- FIXME
      (L (UnfavoriteArticle articleId)) -> undefined
      (R other) -> C $ alg (run . hdl) other ctx
