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

import Article (ArticleR (ArticleWithAuthorProfile))
import Authentication (AuthOf (..))
import Comment (CommentR (CommentWithAuthorProfile))
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
import qualified Data.Semigroup as SG (Last (Last, getLast))
import Field.Email (Email)
import Field.Password (hashPassword, newSalt)
import Field.Slug (titleToSlug)
import Field.Tag (Tag)
import Field.Time (Time)
import qualified GenUUID (E (Generate))
import qualified Relation.ManyToMany (E (GetRelatedLeft, GetRelatedRight, IsRelated, Relate, Unrelate, UnrelateByKeyLeft, UnrelateByKeyRight))
import qualified Relation.ToMany (E (GetRelated, IsRelated, Relate, Unrelate, UnrelateByKey))
import qualified Relation.ToOne (E (GetRelated, Relate, Unrelate))
import Relude.Extra ((.~))
import Storage.Map
  ( ContentOf (..),
    CreateOf (ArticleCreate, CommentCreate),
    HasCreate (CreateOf),
    HasStorage (ContentOf, IdOf),
    IdOf (ArticleId, CommentId, UserId),
    Patch,
    UpdateOf (ArticleUpdate, UserUpdate),
    toArticleId,
    toArticlePatch,
    toUserId,
  )
import qualified Storage.Map (E (DeleteById, GetById, Insert, UpdateById))
import qualified Token (E (CreateToken))
import User (UserR (UserAuthWithToken, UserProfile))
import Util.Error (AlreadyExists (AlreadyExists), CRUD (U), Forbidden (Forbidden), Impossible (Impossible), NotAuthorized (NotAuthorized), NotFound (NotFound))
import Util.Representation (Transform (transform))
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
  UpdateUser :: Patch (UpdateOf "user") -> E m (UserR "authWithToken")
  -- | Follow the user specified by the id.
  --
  -- @since 0.1.0.0
  FollowUser :: IdOf "user" -> E m (UserR "profile")
  -- | Unfollow the user specified by the id.
  --
  -- @since 0.1.0.0
  UnfollowUser :: IdOf "user" -> E m (UserR "profile")
  -- | Create the article specified by the id.
  --
  -- @since 0.1.0.0
  CreateArticle :: CreateOf "article" -> E m (ArticleR "withAuthorProfile")
  -- | Update the authenticated user's own article specified by the id.
  --
  -- @since 0.1.0.0
  UpdateArticle :: IdOf "article" -> Patch (UpdateOf "article") -> E m (ArticleR "withAuthorProfile")
  -- | Delete the authenticated user's own article specified by the id.
  --
  -- @since 0.1.0.0
  DeleteArticle :: IdOf "article" -> E m ()
  -- | Leave a comment to the article specified by the id.
  --
  -- @since 0.1.0.0
  AddCommentToArticle :: IdOf "article" -> CreateOf "comment" -> E m (CommentR "withAuthorProfile")
  -- | Delete the authenticated user's own comment specified by the id from the article.
  --
  -- @since 0.1.0.0
  DeleteComment :: IdOf "article" -> IdOf "comment" -> E m ()
  -- | Favorite the aritcle specified by the id.
  --
  -- @since 0.1.0.0
  FavoriteArticle :: IdOf "article" -> E m (ArticleR "withAuthorProfile")
  -- | Unfavorite the aritcle specified by the id.
  --
  -- @since 0.1.0.0
  UnfavoriteArticle :: IdOf "article" -> E m (ArticleR "withAuthorProfile")
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
  ( Member (Token.E "user") sig,
    Member (Storage.Map.E "user") sig,
    Member (Storage.Map.E "article") sig,
    Member (Storage.Map.E "comment") sig,
    Member (Catch (NotFound (IdOf "user"))) sig,
    Member (Throw (AlreadyExists (IdOf "article"))) sig,
    Member (Throw (AlreadyExists (IdOf "user"))) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Catch (NotFound (IdOf "article"))) sig,
    Member (Throw (NotFound (IdOf "comment"))) sig,
    Member (Throw (NotAuthorized (IdOf "user"))) sig,
    Member (Throw (Forbidden (IdOf "article"))) sig,
    Member (Throw Impossible) sig,
    Member (Current.E Time) sig,
    Member GenUUID.E sig,
    Member (Relation.ManyToMany.E (IdOf "user") "follow" (IdOf "user")) sig,
    Member (Relation.ManyToMany.E (IdOf "user") "favorite" (IdOf "article")) sig,
    Member (Relation.ManyToMany.E (IdOf "article") "taggedBy" Tag) sig,
    Member (Relation.ToMany.E (IdOf "article") "has" (IdOf "comment")) sig,
    Member (Relation.ToMany.E (IdOf "user") "create" (IdOf "article")) sig,
    Member (Relation.ToOne.E Email "of" (IdOf "user")) sig,
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
      let authUserId = toUserId auth
      case action of
        GetCurrentUser -> pure authOut
        UpdateUser update -> do
          -- FIXME GetCurrent (User) should return all(with token?) as it is internal, so that we can avoid looking by authUserId again like below
          orig <- send $ Storage.Map.GetById authUserId
          let m_newEm = getField @"email" update
              m_newName = getField @"username" update
              o_em = getField @"email" orig
              -- NOTE: factor out check(monadic) as effect? register use this code too.
              -- NOTE: right now, only pure validation are checked at boundary.
              checkEmail em
                | em == o_em = pure ()
                | otherwise =
                  send (Relation.ToOne.GetRelated @_ @"of" @(IdOf "user") em) >>= \case
                    Just _ -> throwError $ AlreadyExists em
                    Nothing -> pure ()
              checkUid uid
                | uid == authUserId = pure ()
                | otherwise =
                  catchError @(NotFound (IdOf "user"))
                    (send (Storage.Map.GetById uid) >> throwError (AlreadyExists uid))
                    $ const $ pure ()

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

              send (Relation.ToMany.GetRelated @_ @"create" @(IdOf "article") authUserId)
                >>= traverse_
                  ( \aid -> do
                      send $ Relation.ToMany.Relate @_ @_ @"create" newId aid
                      send $ Storage.Map.UpdateById aid (\a -> a & field' @"author" .~ newId)
                  )
              send $ Relation.ToMany.UnrelateByKey @_ @"create" @(IdOf "article") authUserId

              send (Relation.ManyToMany.GetRelatedLeft @_ @"favorite" @(IdOf "article") authUserId)
                >>= traverse_ (send . Relation.ManyToMany.Relate @_ @_ @"favorite" newId)
              send $ Relation.ManyToMany.UnrelateByKeyLeft @_ @"favorite" @(IdOf "article") authUserId

              send (Relation.ManyToMany.GetRelatedLeft @_ @"follow" @(IdOf "user") authUserId)
                >>= traverse_ (send . Relation.ManyToMany.Relate @_ @_ @"follow" newId)
              send $ Relation.ManyToMany.UnrelateByKeyLeft @_ @"follow" @(IdOf "user") authUserId

              send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf "user") @"follow" authUserId)
                >>= traverse_ (send . \rus -> Relation.ManyToMany.Relate @_ @_ @"follow" rus newId)
              send $ Relation.ManyToMany.UnrelateByKeyRight @_ @(IdOf "user") @"follow" authUserId

              send $ Storage.Map.DeleteById authUserId

          update' <-
            construct $
              build @(HKD (HKD (ContentOf "user") SG.Last) Maybe)
                (pure m_newEm)
                ( case getField @"password" update of
                    Just (SG.Last pwNew) -> Just . SG.Last . hashPassword pwNew <$> sendIO newSalt
                    Nothing -> pure Nothing
                )
                (pure m_newName)
                (pure $ getField @"bio" update)
                (pure $ getField @"image" update)

          newAuth <- case construct $ deconstruct (deconstruct orig) <> update' of
            Nothing -> error "Impossible: Missing field when update"
            Just (construct -> SG.Last r) -> send (Storage.Map.Insert (toUserId r) r) $> transform r

          UserAuthWithToken newAuth <$> send (Token.CreateToken newAuth)
        FollowUser targetUserId -> do
          targetUser <- send $ Storage.Map.GetById targetUserId
          send $ Relation.ManyToMany.Relate @_ @_ @"follow" authUserId targetUserId
          pure $ UserProfile (transform targetUser) True
        UnfollowUser targetUserId -> do
          targetUser <- send $ Storage.Map.GetById targetUserId
          send (Relation.ManyToMany.Unrelate @_ @_ @"follow" authUserId targetUserId)
            $> UserProfile (transform targetUser) False
        CreateArticle (ArticleCreate tt des bd ts) -> do
          let aid = ArticleId $ titleToSlug tt
          catchError @(NotFound (IdOf "article"))
            (send (Storage.Map.GetById aid) >> throwError (AlreadyExists aid))
            $ const $ pure ()
          send $ Relation.ToMany.Relate @_ @_ @"create" authUserId aid
          t <- send $ Current.GetCurrent @Time
          foldMapA (send . Relation.ManyToMany.Relate @_ @_ @"taggedBy" aid) ts
          let a = ArticleContent tt des bd t t $ toUserId auth
          send (Storage.Map.Insert (toArticleId a) a)
            -- FIXME: Follow his own article?
            $> ArticleWithAuthorProfile a [] False 0 (UserProfile auth True)
        UpdateArticle articleId update ->
          send (Storage.Map.GetById articleId) >>= \case
            orig
              | getField @"author" orig /= authUserId -> throwError $ Forbidden U articleId
              | otherwise -> do
                let m_new_aid = ArticleId . titleToSlug . SG.getLast <$> getField @"title" update
                tags <- send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
                fus <- send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf "user") @"favorite" articleId)
                case m_new_aid of
                  Just new_aid
                    | new_aid /= articleId -> do
                      catchError @(NotFound (IdOf "article"))
                        (send (Storage.Map.GetById new_aid) >> throwError (AlreadyExists new_aid))
                        $ const $ pure ()
                      send $ Relation.ToMany.Unrelate @_ @_ @"create" authUserId articleId
                      send $ Relation.ToMany.Relate @_ @_ @"create" authUserId new_aid
                      send (Relation.ToMany.GetRelated @_ @"has" @(IdOf "comment") articleId)
                        >>= traverse_
                          ( \cid -> do
                              send $ Relation.ToMany.Relate @_ @_ @"has" new_aid cid
                              -- send $ Storage.Map.UpdateById cid (\c -> c & field' @"article" .~ new_aid)
                              send $ Storage.Map.UpdateById cid (\c -> c {article = new_aid})
                          )
                      send $ Relation.ToMany.UnrelateByKey @_ @"has" @(IdOf "comment") articleId
                      traverse_ (send . \u -> Relation.ManyToMany.Relate @_ @_ @"favorite" u new_aid) fus
                      send $ Relation.ManyToMany.UnrelateByKeyRight @_ @(IdOf "user") @"favorite" articleId
                      traverse_ (send . Relation.ManyToMany.Relate @_ @_ @"taggedBy" new_aid) tags
                      send $ Relation.ManyToMany.UnrelateByKeyLeft @_ @"taggedBy" @Tag articleId
                      send $ Storage.Map.DeleteById articleId
                    | otherwise -> pure ()
                  Nothing -> pure ()
                a <- case construct $ deconstruct (deconstruct orig) <> toArticlePatch update of
                  Nothing -> error "Impossible: Missing field when update"
                  Just (construct -> SG.Last r) -> send (Storage.Map.Insert (toArticleId r) r) $> r
                ArticleWithAuthorProfile a tags (authUserId `elem` fus) (genericLength fus)
                  <$> send (VisitorAction.GetProfile $ getField @"author" orig)
        DeleteArticle articleId ->
          send (Storage.Map.GetById articleId) >>= \case
            (getField @"author" -> auid)
              | (auid == authUserId) -> do
                send $ Storage.Map.DeleteById articleId
                send $ Relation.ToMany.Unrelate @_ @_ @"create" authUserId articleId
                send $ Relation.ToMany.UnrelateByKey @_ @"has" @(IdOf "comment") articleId
                send $ Relation.ManyToMany.UnrelateByKeyRight @_ @(IdOf "user") @"favorite" articleId
              | otherwise -> throwError $ NotAuthorized authUserId
        AddCommentToArticle articleId (CommentCreate txt) -> do
          t <- send $ Current.GetCurrent @Time
          commentId <- CommentId <$> send GenUUID.Generate
          let a = CommentContent commentId t t txt (toUserId auth) articleId
          send $ Storage.Map.Insert commentId a
          send $ Relation.ToMany.Relate @_ @_ @"has" articleId commentId
          pure $
            CommentWithAuthorProfile commentId t t txt $
              -- FIXME is th current user following himself??
              UserProfile auth True
        DeleteComment articleId commentId -> do
          void $ send $ Storage.Map.GetById articleId
          send (Relation.ToMany.IsRelated @_ @_ @"has" articleId commentId) >>= \case
            False -> throwError $ NotFound commentId
            True ->
              send (Storage.Map.GetById commentId) >>= \case
                (getField @"author" -> auid)
                  | auid == authUserId -> do
                    send $ Storage.Map.DeleteById commentId
                    send $ Relation.ToMany.Unrelate @_ @_ @"has" articleId commentId
                  | otherwise -> throwError $ NotAuthorized authUserId
        FavoriteArticle articleId -> do
          a <- send $ Storage.Map.GetById articleId
          send $ Relation.ManyToMany.Relate @_ @_ @"favorite" authUserId articleId
          let authorId = getField @"author" a
          ArticleWithAuthorProfile a
            <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
            <*> send (Relation.ManyToMany.IsRelated @_ @_ @"favorite" authUserId articleId)
            <*> (genericLength <$> send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf "user") @"favorite" articleId))
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
            <*> (genericLength <$> send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf "user") @"favorite" articleId))
            <*> ( UserProfile . transform
                    <$> send (Storage.Map.GetById authorId)
                    <*> send (Relation.ManyToMany.IsRelated @_ @_ @"follow" authUserId authorId)
                )
        -- FIXME feed order
        FeedArticles -> runNonDetA @[] $ do
          articleId <-
            send (Relation.ManyToMany.GetRelatedLeft @_ @"follow" authUserId)
              >>= oneOf
              >>= send . Relation.ToMany.GetRelated @(IdOf "user") @"create"
              >>= oneOf
          flip (catchError @(NotFound (IdOf "article"))) (const $ throwError $ Impossible "article id not found") $ do
            a <- send $ Storage.Map.GetById articleId
            let authorId = getField @"author" a
            -- TODO: factor out logic for author profile
            ArticleWithAuthorProfile a
              <$> send (Relation.ManyToMany.GetRelatedLeft @_ @"taggedBy" @Tag articleId)
              <*> send (Relation.ManyToMany.IsRelated @_ @_ @"favorite" authUserId articleId)
              <*> (genericLength <$> send (Relation.ManyToMany.GetRelatedRight @_ @(IdOf "user") @"favorite" articleId))
              <*> ( UserProfile . transform
                      <$> send (Storage.Map.GetById authorId)
                      <*> send (Relation.ManyToMany.IsRelated @_ @_ @"follow" authUserId authorId)
                  )
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
