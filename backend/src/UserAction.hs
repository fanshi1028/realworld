{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module UserAction where

import qualified Authentication (E)
import qualified Authentication.Token (E (CreateToken))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import qualified Control.Effect.Reader as R
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified CurrentTime (E (GetCurrentTime))
import Data.Generics.Internal.VL (over)
import Data.Generics.Product (HasField' (field'), getField)
import qualified Data.HashSet as HS (delete, insert)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..), bio, email, image, username)
import Domain.Util.Error (AlreadyExists, NotFound (NotFound), ValidationErr)
import Domain.Util.Representation (Transform (transform), applyPatch)
import qualified GenUUID (E)
import qualified Relation.OneToMany (E (Unrelate))
import qualified Storage (E (DeleteById, GetById, Insert, UpdateById))
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
    Member (Storage.E UserR) sig,
    Member (Storage.E ArticleR) sig,
    Member (Storage.E CommentR) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotFound (UserR "id"))) sig,
    Member (Authentication.E UserR) sig,
    Member (Throw (AlreadyExists (ArticleR "id"))) sig,
    Member (Throw (NotFound (ArticleR "id"))) sig,
    Member (Throw (NotFound (CommentR "id"))) sig,
    Member CurrentTime.E sig,
    Member GenUUID.E sig,
    Member (Relation.OneToMany.E (ArticleR "id") "has" (CommentR "id")) sig,
    Member (R.Reader (UserR "authWithToken")) sig,
    Algebra sig m
  ) =>
  Algebra (UserAction.E :+: sig) (C m)
  where
  alg hdl sig ctx = do
    auth <- R.ask
    authUserId <- case auth of UserAuthWithToken auth' _ -> transform @UserR @"auth" @"id" auth'
    let (%~) = over
    case sig of
      (L action) ->
        (<$ ctx) <$> case action of
          GetCurrentUser -> pure auth
          UpdateUser update ->
            send (Storage.GetById authUserId) >>= \case
              Nothing -> throwError $ NotFound authUserId
              Just (applyPatch update -> user) -> case user of
                V.Failure err -> throwError err
                V.Success new ->
                  send (Storage.UpdateById authUserId $ const new)
                    >>= transform
                    >>= \newAuth -> UserAuthWithToken newAuth <$> send (Authentication.Token.CreateToken newAuth)
          FollowUser userId ->
            send (Storage.GetById userId) >>= \case
              Nothing -> throwError $ NotFound userId
              Just _ -> do
                void $ send $ Storage.UpdateById authUserId (field' @"following" %~ HS.insert userId)
                User {..} <- send $ Storage.UpdateById userId (field' @"followBy" %~ HS.insert authUserId)
                pure $ UserProfile email username bio image True
          UnfollowUser userId ->
            send (Storage.GetById userId) >>= \case
              Nothing -> throwError $ NotFound userId
              Just _ -> do
                void $ send $ Storage.UpdateById authUserId (field' @"following" %~ HS.delete userId)
                User {..} <- send $ Storage.UpdateById userId (field' @"followBy" %~ HS.delete authUserId)
                pure $ UserProfile email username bio image False
          CreateArticle create -> transform create >>= send . Storage.Insert >>= transform
          UpdateArticle articleId update ->
            send (Storage.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just (applyPatch update -> article) -> case article of
                V.Failure err -> throwError err
                V.Success new -> send (Storage.UpdateById articleId $ const new) >>= transform @_ @_ @"withAuthorProfile"
          DeleteArticle articleId ->
            send (Storage.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just _ -> send $ Storage.DeleteById articleId
          AddCommentToArticle articleId cc@(CommentCreate comment) ->
            send (Storage.GetById articleId) >>= \case
              Nothing -> throwError $ NotFound articleId
              Just article -> do
                let authorId = getField @"author" article
                send (Storage.GetById authorId) >>= \case
                  Nothing -> throwError $ NotFound authorId
                  Just User {..} -> do
                    -- FIXME
                    let profile = UserProfile email username bio image undefined
                    commentId <- transform cc
                    time <- send CurrentTime.GetCurrentTime
                    void $ send $ Storage.Insert $ Comment commentId time time comment authorId articleId
                    pure $ CommentWithAuthorProfile commentId time time comment profile
          DeleteComment articleId commentId ->
            send (Storage.GetById commentId) >>= \case
              Nothing -> throwError $ NotFound commentId
              Just _ -> do
                void $ send $ Storage.DeleteById commentId
                send $ Relation.OneToMany.Unrelate (Proxy @"has") articleId commentId
          -- FIXME
          FavoriteArticle articleId -> undefined
          -- FIXME
          UnfavoriteArticle articleId -> undefined
          -- FIXME
          FeedArticles -> undefined
          -- FIXME
          GetCommentsFromArticle articleId -> undefined
      R other -> C $ alg (run . hdl) other ctx
