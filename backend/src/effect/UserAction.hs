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

import Authentication (AuthOf (..), NotAuthorized, NotLogin (NotLogin))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Carrier.NonDet.Church (runNonDetA)
import Control.Effect.Catch (Catch)
import Control.Effect.Error (catchError)
import Control.Effect.NonDet (oneOf)
import qualified Control.Effect.Reader as R (Reader, ask)
import qualified Control.Effect.State as S (State, get, put)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Crypto.Random (DRG, getRandomBytes, withDRG)
import Data.Generic.HKD (Build (build), Construct (construct), HKD, deconstruct)
import Data.Generics.Product (HasField' (field'), getField)
import qualified Data.Semigroup as SG (Last (Last, getLast))
import Data.UUID (UUID)
import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleR (ArticleWithAuthorProfile))
import Domain.Comment (CommentR (CommentWithAuthorProfile))
import Domain.Transform (Transform (transform))
import Domain.User (UserR (UserAuthWithToken, UserProfile))
import Field.Email (Email)
import Field.Password (hashPassword, newSalt)
import Field.Slug (titleToSlug)
import Field.Time (Time)
import InMem.Relation
  ( ManyToMany
      ( getRelatedLeftManyToMany,
        getRelatedRightManyToMany,
        isRelatedManyToMany,
        relateManyToMany,
        unrelateByKeyLeftManyToMany,
        unrelateByKeyRightManyToMany,
        unrelateManyToMany
      ),
    ManyToManyRelationE,
    ToMany (getRelatedToMany, isRelatedToMany, unrelateByKeyToMany, unrelateToMany),
    ToManyRelationE,
    ToOne (getRelatedToOne, relateToOne, unrelateToOne),
    ToOneRelationE,
    relateToMany,
  )
import InMem.Storage (MapInMemE, deleteByIdMapInMem, getByIdMapInMem, insertMapInMem, updateByIdMapInMem)
import InMem.Storage.Error (AlreadyExists (AlreadyExists), NotFound (NotFound))
import InMem.Storage.Map (CRUD (D, U), ContentOf (..), CreateOf (ArticleCreate, CommentCreate), Forbidden (Forbidden), HasCreate (CreateOf), HasStorage (ContentOf, IdOf), IdAlreadyExists, IdNotFound, IdOf (ArticleId, CommentId, UserId), Patch, UpdateOf, toArticleId, toArticlePatch, toUserId)
import qualified OptionalAuthAction (E (GetProfile))
import Relude.Extra ((.~), (^.))

-- * Effect

-- | @since 0.3.0.0
-- Actions that can only be carried out by __authenticated__ users.
data E (m :: Type -> Type) a where
  -- | @since 0.1.0.0
  -- Get the info of the current authenticated user.
  GetCurrentUser :: E m (UserR "authWithToken")
  -- | @since 0.3.0.0
  -- Update the profile of the current authenticated user.
  UpdateUser :: Patch (UpdateOf 'User) -> E m (AuthOf 'User)
  -- | @since 0.1.0.0
  -- Follow the user specified by the id.
  FollowUser :: IdOf 'User -> E m (UserR "profile")
  -- | @since 0.1.0.0
  -- Unfollow the user specified by the id.
  UnfollowUser :: IdOf 'User -> E m (UserR "profile")
  -- | @since 0.1.0.0
  -- Create the article specified by the id.
  CreateArticle :: CreateOf 'Article -> E m (ArticleR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Update the authenticated user's own article specified by the id.
  UpdateArticle :: IdOf 'Article -> Patch (UpdateOf 'Article) -> E m (ArticleR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Delete the authenticated user's own article specified by the id.
  DeleteArticle :: IdOf 'Article -> E m ()
  -- | @since 0.1.0.0
  -- Leave a comment to the article specified by the id.
  AddCommentToArticle :: IdOf 'Article -> CreateOf 'Comment -> E m (CommentR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Delete the authenticated user's own comment specified by the id from the article.
  DeleteComment :: IdOf 'Article -> IdOf 'Comment -> E m ()
  -- | @since 0.1.0.0
  -- Favorite the aritcle specified by the id.
  FavoriteArticle :: IdOf 'Article -> E m (ArticleR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Unfavorite the aritcle specified by the id.
  UnfavoriteArticle :: IdOf 'Article -> E m (ArticleR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Get articles feed recommendated for the authenticated user.
  FeedArticles :: E m [ArticleR "withAuthorProfile"]

-- * Carrirer

-- | @since 0.1.0.0
newtype C gen m a = C
  { -- | @since 0.1.0.0
    run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.2.0.0
instance
  ( MapInMemE 'User sig,
    MapInMemE 'Article sig,
    MapInMemE 'Comment sig,
    Member (Catch (IdNotFound 'User)) sig,
    Member (Throw (IdAlreadyExists 'Article)) sig,
    Member (Throw (IdAlreadyExists 'User)) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Catch (IdNotFound 'Article)) sig,
    Member (Throw (IdNotFound 'Comment)) sig,
    Member (Throw (Forbidden 'U 'Article)) sig,
    Member (Throw (Forbidden 'D 'Article)) sig,
    Member (Throw (Forbidden 'D 'Comment)) sig,
    Member (Throw Text) sig,
    Member (R.Reader Time) sig,
    Member (R.Reader UUID) sig,
    ManyToManyRelationE "UserFollowUser" sig,
    ManyToManyRelationE "UserFavoriteArticle" sig,
    ManyToManyRelationE "ArticleTaggedByTag" sig,
    ToManyRelationE "ArticleHasComment" sig,
    ToManyRelationE "UserCreateComment" sig,
    ToManyRelationE "UserCreateArticle" sig,
    ToOneRelationE "EmailOfUser" sig,
    Member (Throw (NotLogin 'User)) sig,
    Member (Throw (NotAuthorized 'User)) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig,
    Member OptionalAuthAction.E sig,
    Member (S.State gen) sig,
    DRG gen,
    Algebra sig m
  ) =>
  Algebra (UserAction.E :+: sig) (C gen m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      authOut@(UserAuthWithToken auth@(toUserId -> authUserId) _) <-
        R.ask >>= \case
          Just auth -> pure auth
          Nothing -> throwError $ NotLogin @'User
      case action of
        GetCurrentUser -> pure authOut
        UpdateUser update -> do
          -- FIXME GetCurrent (User) should return all(with token?) as it is internal, so that we can avoid looking by authUserId again like below
          orig <- getByIdMapInMem authUserId
          let m_newEm = getField @"email" update
              m_newName = getField @"username" update
              o_em = getField @"email" orig
              -- NOTE: factor out check(monadic) as effect? register use this code too.
              -- NOTE: right now, only pure validation are checked at boundary.
              checkEmail em
                | em == o_em = pure ()
                | otherwise =
                  getRelatedToOne @"EmailOfUser" em >>= \case
                    Just _ -> throwError $ AlreadyExists em
                    Nothing -> pure ()
              checkUid uid
                | uid == authUserId = pure ()
                | otherwise =
                  catchError @(IdNotFound 'User)
                    (getByIdMapInMem uid >> throwError (AlreadyExists uid))
                    $ const $ pure ()

          case m_newName of
            Nothing -> do
              case m_newEm of
                Just (SG.Last newEm) -> do
                  checkEmail newEm
                  unrelateToOne @"EmailOfUser" o_em authUserId
                  relateToOne @"EmailOfUser" newEm authUserId
                Nothing -> pure ()
            Just (SG.Last (UserId -> newId)) -> do
              checkUid newId
              unrelateToOne @"EmailOfUser" o_em authUserId
              case m_newEm of
                Just (SG.Last newEm) -> do
                  checkEmail newEm
                  relateToOne @"EmailOfUser" newEm newId
                Nothing -> relateToOne @"EmailOfUser" o_em newId

              getRelatedToMany @"UserCreateArticle" authUserId
                >>= traverse_
                  ( \aid -> do
                      relateToMany @"UserCreateArticle" newId aid
                      updateByIdMapInMem aid (field' @"author" .~ newId)
                  )
              unrelateByKeyToMany @"UserCreateArticle" authUserId

              getRelatedToMany @"UserCreateComment" authUserId
                >>= traverse_
                  ( \cid -> do
                      relateToMany @"UserCreateComment" newId cid
                      updateByIdMapInMem cid (& field' @"author" .~ newId)
                  )
              unrelateByKeyToMany @"UserCreateComment" authUserId

              getRelatedLeftManyToMany @"UserFavoriteArticle" authUserId
                >>= traverse_ (relateManyToMany @"UserFavoriteArticle" newId)
              unrelateByKeyLeftManyToMany @"UserFavoriteArticle" authUserId

              getRelatedLeftManyToMany @"UserFollowUser" authUserId
                >>= traverse_ (relateManyToMany @"UserFollowUser" newId)
              unrelateByKeyLeftManyToMany @"UserFollowUser" authUserId

              getRelatedRightManyToMany @"UserFollowUser" authUserId
                >>= traverse_ (\rus -> relateManyToMany @"UserFollowUser" rus newId)
              unrelateByKeyRightManyToMany @"UserFollowUser" authUserId

              void $ deleteByIdMapInMem authUserId

          update' <-
            construct $
              build @(HKD (HKD (ContentOf 'User) SG.Last) Maybe)
                (pure m_newEm)
                ( case getField @"password" update of
                    Just (SG.Last pwNew) -> do
                      g <- S.get @gen
                      let (salt, g') = withDRG g $ newSalt <$> getRandomBytes 16
                      S.put g'
                      pure $ Just $ SG.Last $ hashPassword pwNew salt
                    Nothing -> pure Nothing
                )
                (pure m_newName)
                (pure $ getField @"bio" update)
                (pure $ getField @"image" update)

          case construct $ deconstruct (deconstruct orig) <> update' of
            Nothing -> error "Impossible: Missing field when update"
            Just (construct -> SG.Last r) -> insertMapInMem (toUserId r) r $> transform r
        FollowUser targetUserId -> do
          targetUser <- getByIdMapInMem targetUserId
          relateManyToMany @"UserFollowUser" authUserId targetUserId
          pure $ UserProfile (transform targetUser) True
        UnfollowUser targetUserId -> do
          targetUser <- getByIdMapInMem targetUserId
          unrelateManyToMany @"UserFollowUser" authUserId targetUserId
            $> UserProfile (transform targetUser) False
        CreateArticle (ArticleCreate tt des bd ts) -> do
          let aid = ArticleId $ titleToSlug tt
          catchError @(IdNotFound 'Article)
            (getByIdMapInMem aid >> throwError (AlreadyExists aid))
            $ const $ pure ()
          relateToMany @"UserCreateArticle" authUserId aid
          t <- R.ask @Time
          foldMapA (relateManyToMany @"ArticleTaggedByTag" aid) ts
          let a = ArticleContent tt des bd t t authUserId
          insertMapInMem (toArticleId a) a
            -- FIXME: Follow his own article?
            $> ArticleWithAuthorProfile a [] False 0 (UserProfile auth True)
        UpdateArticle articleId update ->
          getByIdMapInMem articleId >>= \case
            orig
              | getField @"author" orig /= authUserId -> throwError $ Forbidden @'U articleId
              | otherwise -> do
                let m_new_aid = ArticleId . titleToSlug . SG.getLast <$> getField @"title" update
                tags <- getRelatedLeftManyToMany @"ArticleTaggedByTag" articleId
                fus <- getRelatedRightManyToMany @"UserFavoriteArticle" articleId
                case m_new_aid of
                  Just new_aid
                    | new_aid /= articleId -> do
                      catchError @(IdNotFound 'Article)
                        (getByIdMapInMem new_aid >> throwError (AlreadyExists new_aid))
                        $ const $ pure ()
                      unrelateToMany @"UserCreateArticle" authUserId articleId
                      relateToMany @"UserCreateArticle" authUserId new_aid
                      getRelatedToMany @"ArticleHasComment" articleId
                        >>= traverse_
                          ( \cid -> do
                              relateToMany @"ArticleHasComment" new_aid cid
                              updateByIdMapInMem cid $ \c -> c {article = new_aid}
                          )
                      unrelateByKeyToMany @"ArticleHasComment" articleId
                      traverse_ (\u -> relateManyToMany @"UserFavoriteArticle" u new_aid) fus
                      unrelateByKeyRightManyToMany @"UserFavoriteArticle" articleId
                      traverse_ (relateManyToMany @"ArticleTaggedByTag" new_aid) tags
                      unrelateByKeyLeftManyToMany @"ArticleTaggedByTag" articleId
                      void $ deleteByIdMapInMem articleId
                    | otherwise -> pure ()
                  Nothing -> pure ()
                a <- case construct $ deconstruct (deconstruct orig) <> toArticlePatch update of
                  Nothing -> error "Impossible: Missing field when update"
                  Just (construct -> SG.Last r) -> insertMapInMem (toArticleId r) r $> r
                ArticleWithAuthorProfile a tags (authUserId `elem` fus) (genericLength fus)
                  <$> send (OptionalAuthAction.GetProfile $ getField @"author" orig)
        DeleteArticle articleId ->
          getByIdMapInMem articleId >>= \case
            (getField @"author" -> auid)
              | (auid == authUserId) -> do
                void $ deleteByIdMapInMem articleId
                unrelateToMany @"UserCreateArticle" authUserId articleId

                getRelatedToMany @"ArticleHasComment" articleId
                  >>= traverse_
                    ( \cid -> do
                        uid <- (^. field' @"author") <$> getByIdMapInMem cid
                        unrelateToMany @"UserCreateComment" uid cid
                        deleteByIdMapInMem cid
                    )
                unrelateByKeyToMany @"ArticleHasComment" articleId

                unrelateByKeyRightManyToMany @"UserFavoriteArticle" articleId
              | otherwise -> throwError $ Forbidden @'D articleId
        AddCommentToArticle articleId (CommentCreate txt) -> do
          t <- R.ask @Time
          commentId <- CommentId <$> R.ask
          let a = CommentContent commentId t t txt authUserId articleId
          insertMapInMem commentId a
          relateToMany @"ArticleHasComment" articleId commentId
          relateToMany @"UserCreateComment" authUserId commentId
          pure $
            CommentWithAuthorProfile commentId t t txt $
              -- FIXME is th current user following himself??
              UserProfile auth True
        DeleteComment articleId commentId -> do
          void $ getByIdMapInMem articleId
          isRelatedToMany @"ArticleHasComment" articleId commentId >>= \case
            False -> throwError $ NotFound commentId
            True ->
              getByIdMapInMem commentId >>= \case
                (getField @"author" -> auid)
                  | auid == authUserId -> do
                    void $ deleteByIdMapInMem commentId
                    unrelateToMany @"ArticleHasComment" articleId commentId
                    unrelateToMany @"UserCreateComment" authUserId commentId
                  | otherwise -> throwError $ Forbidden @'D commentId
        FavoriteArticle articleId -> do
          a@(getField @"author" -> authorId) <- getByIdMapInMem articleId
          relateManyToMany @"UserFavoriteArticle" authUserId articleId
          ArticleWithAuthorProfile a
            <$> getRelatedLeftManyToMany @"ArticleTaggedByTag" articleId
            <*> isRelatedManyToMany @"UserFavoriteArticle" authUserId articleId
            <*> (genericLength <$> getRelatedRightManyToMany @"UserFavoriteArticle" articleId)
            <*> send (OptionalAuthAction.GetProfile authorId)
        UnfavoriteArticle articleId -> do
          a@(getField @"author" -> authorId) <- getByIdMapInMem articleId
          unrelateManyToMany @"UserFavoriteArticle" authUserId articleId
          ArticleWithAuthorProfile a
            <$> getRelatedLeftManyToMany @"ArticleTaggedByTag" articleId
            <*> isRelatedManyToMany @"UserFavoriteArticle" authUserId articleId
            <*> (genericLength <$> getRelatedRightManyToMany @"UserFavoriteArticle" articleId)
            <*> send (OptionalAuthAction.GetProfile authorId)
        -- FIXME feed order
        FeedArticles -> runNonDetA @[] $ do
          articleId <-
            getRelatedLeftManyToMany @"UserFollowUser" authUserId
              >>= oneOf
              >>= getRelatedToMany @"UserCreateArticle"
              >>= oneOf
          flip (catchError @(IdNotFound 'Article)) (const $ throwError @Text "impossible: article id not found") $ do
            a@(getField @"author" -> authorId) <- getByIdMapInMem articleId
            ArticleWithAuthorProfile a
              <$> getRelatedLeftManyToMany @"ArticleTaggedByTag" articleId
              <*> isRelatedManyToMany @"UserFavoriteArticle" authUserId articleId
              <*> (genericLength <$> getRelatedRightManyToMany @"UserFavoriteArticle" articleId)
              <*> send (OptionalAuthAction.GetProfile authorId)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
