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

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (Catch)
import Control.Effect.Error (catchError)
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Current (E (GetCurrent))
import Data.Generic.HKD (Build (build), Construct (construct), HKD, deconstruct)
import Data.Generics.Product (HasField' (field'), getField)
import Data.Password.Argon2 (hashPassword)
import qualified Data.Semigroup as SG (Last (Last, getLast))
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists (AlreadyExists), CRUD (U), Forbidden (Forbidden), Impossible (Impossible), NotAuthorized (NotAuthorized), NotFound (NotFound))
import Domain.Util.Field (Email, Tag, Time, titleToSlug)
import Domain.Util.Representation (Transform (transform))
import qualified GenUUID (E (Generate))
import qualified Relation.ManyToMany (E (GetRelatedLeft, GetRelatedRight, IsRelated, Relate, Unrelate, UnrelateByKeyLeft, UnrelateByKeyRight))
import qualified Relation.ToMany (E (GetRelated, IsRelated, Relate, Unrelate, UnrelateByKey))
import qualified Relation.ToOne (E (GetRelated, Relate, Unrelate))
import Relude.Extra ((.~))
import qualified Storage.Map (E (DeleteById, GetById, Insert, UpdateById))
import qualified Token (E (CreateToken))
import qualified VisitorAction (E (GetProfile))

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
    Member (Catch (NotFound (UserR "id"))) sig,
    Member (Throw (AlreadyExists (ArticleR "id"))) sig,
    Member (Throw (AlreadyExists (UserR "id"))) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Catch (NotFound (ArticleR "id"))) sig,
    Member (Throw (NotFound (CommentR "id"))) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Throw (Forbidden (ArticleR "id"))) sig,
    Member (Throw Impossible) sig,
    Member (Current.E Time) sig,
    Member GenUUID.E sig,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Member (Relation.ManyToMany.E (UserR "id") "favorite" (ArticleR "id")) sig,
    Member (Relation.ManyToMany.E (ArticleR "id") "taggedBy" Tag) sig,
    Member (Relation.ToMany.E (ArticleR "id") "has" (CommentR "id")) sig,
    Member (Relation.ToMany.E (UserR "id") "create" (ArticleR "id")) sig,
    Member (Relation.ToOne.E Email "of" (UserR "id")) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member VisitorAction.E sig,
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
          -- FIXME GetCurrent (User) should return all(with token?) as it is internal, so that we can avoid looking by authUserId again like below
          send (Storage.Map.GetById authUserId) >>= \orig -> do
            let m_newEm = getField @"email" update
                m_newName = getField @"username" update
                o_em = getField @"email" orig
                -- NOTE: factor out check(monadic) as effect? register use this code too.
                -- NOTE: right now, only pure validation are checked at boundary.
                checkEmail em
                  | em == o_em = pure ()
                  | otherwise =
                    send (Relation.ToOne.GetRelated @_ @"of" @(UserR "id") em) >>= \case
                      Just _ -> throwError $ AlreadyExists em
                      Nothing -> pure ()
                checkUid uid
                  | uid == authUserId = pure ()
                  | otherwise =
                    ( send (Storage.Map.GetById uid) >> throwError (AlreadyExists uid)
                    )
                      `catchError` const @_ @(NotFound (UserR "id")) (pure ())

            case m_newName of
              Nothing -> do
                case m_newEm of
                  Just (SG.Last newEm) -> do
                    checkEmail newEm
                    send $ Relation.ToOne.Unrelate @_ @_ @"of" o_em authUserId
                    send $ Relation.ToOne.Relate @_ @_ @"of" newEm authUserId
                  Nothing -> pure ()
              Just (SG.Last (UserId -> newId)) -> do
                checkUid newId
                send $ Relation.ToOne.Unrelate @_ @_ @"of" o_em authUserId
                case m_newEm of
                  Just (SG.Last newEm) -> do
                    checkEmail newEm
                    send $ Relation.ToOne.Relate @_ @_ @"of" newEm newId
                  Nothing -> send $ Relation.ToOne.Relate @_ @_ @"of" o_em newId

                send (Relation.ToMany.GetRelated @_ @"create" @(ArticleR "id") authUserId)
                  >>= traverse_
                    ( \aid -> do
                        send $ Relation.ToMany.Relate @_ @_ @"create" newId aid
                        send $ Storage.Map.UpdateById aid (\a -> a & field' @"author" .~ newId)
                    )
                send $ Relation.ToMany.UnrelateByKey @_ @"create" @(ArticleR "id") authUserId

                send (Relation.ManyToMany.GetRelatedLeft @_ @"favorite" @(ArticleR "id") authUserId)
                  >>= traverse_ (send . Relation.ManyToMany.Relate @_ @_ @"favorite" newId)
                send $ Relation.ManyToMany.UnrelateByKeyLeft @_ @"favorite" @(ArticleR "id") authUserId

                send (Relation.ManyToMany.GetRelatedLeft @_ @"follow" @(UserR "id") authUserId)
                  >>= traverse_ (send . Relation.ManyToMany.Relate @_ @_ @"follow" newId)
                send $ Relation.ManyToMany.UnrelateByKeyLeft @_ @"follow" @(UserR "id") authUserId

                send (Relation.ManyToMany.GetRelatedRight @_ @(UserR "id") @"follow" authUserId)
                  >>= traverse_ (send . \rus -> Relation.ManyToMany.Relate @_ @_ @"follow" rus newId)
                send $ Relation.ManyToMany.UnrelateByKeyRight @_ @(UserR "id") @"follow" authUserId

                send $ Storage.Map.DeleteById authUserId

            update' <-
              construct $
                build @(HKD (HKD (UserR "all") SG.Last) Maybe)
                  (pure m_newEm)
                  ( case getField @"password" update of
                      Just (SG.Last pwNew) -> do
                        hpw <- sendIO $ hashPassword pwNew
                        pure $ Just $ SG.Last hpw
                      Nothing -> pure Nothing
                  )
                  (pure m_newName)
                  (pure $ getField @"bio" update)
                  (pure $ getField @"image" update)

            newAuth <- case construct $ deconstruct (deconstruct orig) <> update' of
              Nothing -> error "Impossible: Missing field when update"
              Just (construct -> SG.Last r) -> send (Storage.Map.Insert r) $> transform r

            UserAuthWithToken newAuth <$> send (Token.CreateToken newAuth)
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
          (send (Storage.Map.GetById aid) >> throwError (AlreadyExists aid))
            `catchError` (const @_ @(NotFound (ArticleR "id")) $ pure ())
          send $ Relation.ToMany.Relate @_ @(ArticleR "id") @"create" authUserId aid
          t <- send $ Current.GetCurrent @Time
          foldMapA (send . Relation.ManyToMany.Relate @(ArticleR "id") @_ @"taggedBy" (transform create)) ts
          let a = Article tt des bd t t $ transform auth
          send (Storage.Map.Insert a)
            -- FIXME: Follow his own article?
            $> ArticleWithAuthorProfile a [] False 0 (UserProfile auth True)
        UpdateArticle articleId (ArticleUpdate update) ->
          send (Storage.Map.GetById articleId) >>= \case
            orig
              | getField @"author" orig /= authUserId -> throwError $ Forbidden U articleId
              | otherwise -> do
                let m_new_aid = ArticleId . titleToSlug . SG.getLast <$> getField @"title" update
                tags <- send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
                fus <- send (Relation.ManyToMany.GetRelatedRight @_ @(UserR "id") @"favorite" articleId)
                case m_new_aid of
                  Just new_aid
                    | new_aid /= articleId -> do
                      (send (Storage.Map.GetById new_aid) >> throwError (AlreadyExists new_aid))
                        `catchError` (const @_ @(NotFound (ArticleR "id")) $ pure ())
                      send $ Relation.ToMany.Unrelate @_ @_ @"create" authUserId articleId
                      send $ Relation.ToMany.Relate @_ @_ @"create" authUserId new_aid
                      send (Relation.ToMany.GetRelated @_ @"has" @(CommentR "id") articleId)
                        >>= traverse_
                          ( \cid -> do
                              send $ Relation.ToMany.Relate @_ @_ @"has" new_aid cid
                              send $ Storage.Map.UpdateById cid (\c -> c & field' @"article" .~ new_aid)
                          )
                      send $ Relation.ToMany.UnrelateByKey @_ @"has" @(CommentR "id") articleId
                      traverse_ (send . \u -> Relation.ManyToMany.Relate @_ @_ @"favorite" u new_aid) fus
                      send $ Relation.ManyToMany.UnrelateByKeyRight @_ @(UserR "id") @"favorite" articleId
                      traverse_ (send . Relation.ManyToMany.Relate @_ @_ @"taggedBy" new_aid) tags
                      send $ Relation.ManyToMany.UnrelateByKeyLeft @_ @"taggedBy" @Tag articleId
                      send $ Storage.Map.DeleteById articleId
                    | otherwise -> pure ()
                  Nothing -> pure ()
                a <- case construct $ deconstruct (deconstruct orig) <> update of
                  Nothing -> error "Impossible: Missing field when update"
                  Just (construct -> SG.Last r) -> send (Storage.Map.Insert r) $> r
                ArticleWithAuthorProfile a tags (authUserId `elem` fus) (fromIntegral $ length fus)
                  <$> send (VisitorAction.GetProfile $ getField @"author" orig)
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
          ArticleWithAuthorProfile a
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
          ArticleWithAuthorProfile a
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
                  ArticleWithAuthorProfile a
                    <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
                    <*> send (Relation.ManyToMany.IsRelated @_ @_ @"favorite" authUserId articleId)
                    <*> (fromIntegral . length <$> send (Relation.ManyToMany.GetRelatedRight @_ @(UserR "id") @"favorite" articleId))
                    <*> ( UserProfile . transform
                            <$> send (Storage.Map.GetById authorId)
                            <*> send (Relation.ManyToMany.IsRelated @_ @_ @"follow" authUserId authorId)
                        )
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
