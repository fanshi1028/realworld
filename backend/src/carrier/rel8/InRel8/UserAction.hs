{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier of users' action with postgres storage
--
-- @since 0.4.0.0
module InRel8.UserAction where

import Authentication (AuthenticationE (GetCurrentAuth))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import CreateSalt (CreateSaltE (CreateSalt))
import Data.Authentication.HasAuth (AuthOf (..), NotAuthorized)
import Data.Domain (Domain (Article, Comment, User))
import Data.Domain.Article (ArticleWithAuthorProfile)
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken), UserProfile)
import Data.Field.Email (Email)
import Data.Field.Password (hashPassword)
import Data.Field.Slug (titleToSlug)
import Data.Generics.Product (getField)
import qualified Data.Semigroup as SG (Last (Last, getLast))
import Data.Storage.Error (AlreadyExists (AlreadyExists), NotFound (NotFound))
import Data.Storage.Map
  ( CRUD (D, U),
    Forbidden (Forbidden),
    IdAlreadyExists,
    IdNotFound,
    IdOf (ArticleId, CommentId, UserId),
    toUserId,
  )
import Data.Storage.Map.HasCreate (CreateOf (ArticleCreate, CommentCreate))
import Data.Token.HasToken (TokenOf)
import Data.UUID (UUID)
import Data.Util.Impossible (Impossible (Impossible))
import InRel8.Sql (SqlInRel8E (SqlSelect), deleteOneRow, insertOneRow, insertRows, toggleOff, toggleOn, updateOneRow)
import InRel8.Storage (getArticleById, getArticles, getAuthorForArticle, getCommentById, getProfile, getUserById, mkArticle, mkAuth, mkComment, mkProfile)
import InRel8.Storage.Schema.Article (ArticleRel8 (ArticleRel8), articleSchema)
import InRel8.Storage.Schema.Article as ArticleRel8 (author, slug)
import InRel8.Storage.Schema.ArticleHasTag (ArticleHasTagRel8 (ArticleHasTagRel8), articleHasTagSchema)
import InRel8.Storage.Schema.Comment as CommentRel8 (CommentRel8 (CommentRel8, author), article, commentSchema, id)
import InRel8.Storage.Schema.Tag as TagRel8 (TagRel8 (TagRel8), tag, tagSchema)
import InRel8.Storage.Schema.User as UserRel8 (UserRel8 (UserRel8, email), userSchema, username)
import InRel8.Storage.Schema.UserFavoriteArticle as UserFavoriteArticleRel8 (UserFavoriteArticleRel8 (UserFavoriteArticleRel8, favoritedBy, favoriting), userFavoriteArticleSchema)
import InRel8.Storage.Schema.UserFollowUser as UserFollowUserRel8 (UserFollowUserRel8 (UserFollowUserRel8), followedBy, following, userFollowUserSchema)
import OptionalAuthAction (OptionalAuthActionE)
import Rel8 (Expr, each, filter, lit, select, unsafeDefault, (&&.), (/=.), (==.))
import UserAction
  ( UserActionE
      ( AddCommentToArticle,
        CreateArticle,
        DeleteArticle,
        DeleteComment,
        FavoriteArticle,
        FollowUser,
        GetCurrentUser,
        UnfavoriteArticle,
        UnfollowUser,
        UpdateArticle,
        UpdateUser
      ),
  )

-- | @since 0.4.0.0
newtype UserActionInRel8C m a = UserActionInRel8C
  { -- | @since 0.4.0.0
    runUserActionInRel8 :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.4.0.0
instance
  ( Member (Catch (IdNotFound 'User)) sig,
    Member (Throw (IdAlreadyExists 'User)) sig,
    Member (Throw (IdAlreadyExists 'Article)) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Throw (IdNotFound 'Article)) sig,
    Member (Catch (IdNotFound 'Article)) sig,
    Member (Throw (IdNotFound 'Comment)) sig,
    Member (Throw (Forbidden 'U 'Article)) sig,
    Member (Throw (Forbidden 'D 'Article)) sig,
    Member (Throw (Forbidden 'D 'Comment)) sig,
    Member (Throw Text) sig,
    Member (Throw Impossible) sig,
    Member (Throw (NotAuthorized 'User)) sig,
    Member (AuthenticationE 'User) sig,
    Member (R.Reader (Maybe (TokenOf 'User))) sig,
    Member (R.Reader UUID) sig,
    Member OptionalAuthActionE sig,
    Member CreateSaltE sig,
    Member SqlInRel8E sig,
    Algebra sig m
  ) =>
  Algebra (UserActionE :+: sig) (UserActionInRel8C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      auth@(toUserId -> authUserId) <-
        send (GetCurrentAuth @'User)
          >>= maybe (throwError $ Impossible "not login would not get here") pure
      user <-
        send (SqlSelect . select . getUserById $ lit authUserId) >>= \case
          [] -> throwError $ Impossible "authed user not found"
          [u] -> pure u
          _ -> throwError $ Impossible $ "got more than one user from the authed user id : " <> show authUserId
      let checkId getById id' =
            send (SqlSelect . select $ getById $ lit id') >>= \case
              [] -> throwError $ NotFound id'
              [c] -> pure c
              _ -> throwError $ Impossible $ "got more than one row from id " <> show id'
          checkCommentId cid = checkId getCommentById cid
          checkArticleId aid = checkId getArticleById aid
      let getProfileById :: Expr (IdOf 'User) -> UserActionInRel8C m UserProfile
          getProfileById userId =
            send
              ( SqlSelect . select $
                  getUserById userId
                    >>= getProfile (Just auth)
              )
              >>= \case
                [] -> throwError $ Impossible "not found user profile"
                [r] -> pure $ mkProfile r
                _ -> throwError $ Impossible "got more than one user profile from user id"
          getArticleWithAuthorProfileById :: Expr (IdOf 'Article) -> UserActionInRel8C m ArticleWithAuthorProfile
          getArticleWithAuthorProfileById articleId = do
            send
              ( SqlSelect . select $
                  getArticles (Just auth)
                    >>= Rel8.filter (\(a, _, _, _, _, _) -> ArticleRel8.slug a ==. articleId)
              )
              >>= \case
                [] -> throwError $ Impossible "article not found"
                [r] -> pure $ mkArticle r
                _ -> throwError $ Impossible "got more than one article from article id"
          updateHelper old mNew = maybe old (lit . SG.getLast) mNew
      case action of
        GetCurrentUser -> R.ask >>= maybe (throwError $ Impossible "Missing token") (pure . UserAuthWithToken auth)
        UpdateUser update' ->
          do
            mPw <- case getField @"password" update' of
              Just (SG.Last pwNew) -> send CreateSalt <&> Just . hashPassword pwNew
              Nothing -> pure Nothing
            updateOneRow
              userSchema
              ( \(UserRel8 o_uid o_em o_pw o_bio o_img o_ct _) ->
                  let uid' = case getField @"username" update' of
                        Nothing -> o_uid
                        Just (SG.Last (UserId -> newId)) -> lit newId
                      em' = updateHelper o_em $ getField @"email" update'
                      bio' = updateHelper o_bio $ getField @"bio" update'
                      img' = updateHelper o_img $ getField @"image" update'
                   in UserRel8 uid' em' (maybe o_pw lit mPw) bio' img' o_ct unsafeDefault
              )
              (UserRel8.username >>> (==. lit authUserId))
              >>= \case
                Just a -> pure $ mkAuth a
                Nothing -> do
                  case getField @"email" update' of
                    Nothing -> pure ()
                    Just (SG.Last newEm) ->
                      send
                        ( SqlSelect . select $
                            each userSchema
                              >>= Rel8.filter ((==. lit newEm) . UserRel8.email)
                        )
                        >>= \case
                          [] -> pure ()
                          _ -> throwError $ AlreadyExists newEm
                  case getField @"username" update' of
                    Nothing -> pure ()
                    Just (SG.Last (UserId -> newUid)) ->
                      send
                        ( SqlSelect . select $
                            each userSchema
                              >>= Rel8.filter ((==. lit newUid) . UserRel8.username)
                        )
                        >>= \case
                          [] -> pure ()
                          _ -> throwError $ AlreadyExists newUid
                  throwError $ Impossible "user update failed"
        FollowUser (lit -> targetUserId) ->
          toggleOn
            userFollowUserSchema
            (UserFollowUserRel8 (lit authUserId) targetUserId unsafeDefault)
            $ getProfileById targetUserId
        UnfollowUser (lit -> targetUserId) -> do
          toggleOff
            userFollowUserSchema
            ( \r ->
                UserFollowUserRel8.followedBy r
                  ==. lit authUserId &&. UserFollowUserRel8.following r
                  ==. targetUserId
            )
          getProfileById targetUserId
        CreateArticle (ArticleCreate tt@(ArticleId . titleToSlug -> aid) des bd ts) -> do
          a <-
            insertOneRow
              articleSchema
              ( ArticleRel8
                  (lit aid)
                  (lit tt)
                  (lit des)
                  (lit bd)
                  (lit authUserId)
                  unsafeDefault
                  unsafeDefault
              )
              Prelude.id
              $ send (SqlSelect . select $ getArticleById $ lit aid) >>= \case
                [] -> throwError $ Impossible "insert row failed"
                [_] -> throwError $ AlreadyExists aid
                _ -> throwError $ Impossible $ "got more than one article form article id " <> show aid
          tids <-
            insertRows
              tagSchema
              (ts <&> \t -> TagRel8 (lit t) unsafeDefault)
              TagRel8.tag
          _ <-
            insertRows
              articleHasTagSchema
              (tids <&> \t -> ArticleHasTagRel8 (lit aid) (lit t) unsafeDefault)
              Prelude.id
          -- FIXME: Follow his own article?
          pure $ mkArticle (a, user, tids, True, False, 0)
        UpdateArticle articleId update' ->
          updateOneRow
            articleSchema
            ( \(ArticleRel8 o_slug o_tt o_des o_bd o_au o_ct _) ->
                let (tt', slug') =
                      ( updateHelper o_tt
                          &&& maybe o_slug (lit . ArticleId . titleToSlug . SG.getLast)
                      )
                        $ getField @"title" update'
                    des' = updateHelper o_des $ getField @"description" update'
                    bd' = updateHelper o_bd $ getField @"body" update'
                 in ArticleRel8 slug' tt' des' bd' o_au o_ct unsafeDefault
            )
            ( \a ->
                ArticleRel8.author a ==. lit authUserId
                  &&. ArticleRel8.slug a ==. lit articleId
            )
            >>= \case
              Just (lit . ArticleRel8.slug -> aid) -> getArticleWithAuthorProfileById aid
              Nothing -> do
                _ <- checkArticleId articleId
                send
                  ( SqlSelect . select $
                      getArticleById (lit articleId)
                        >>= Rel8.filter
                          (ArticleRel8.author >>> (/=. lit authUserId))
                  )
                  >>= \case
                    [] -> throwError $ Forbidden @'U articleId
                    _ -> pure ()
                case getField @"title" update' of
                  Nothing -> pure ()
                  Just (SG.Last (ArticleId . titleToSlug -> aid')) ->
                    send (SqlSelect . select $ getArticleById (lit aid'))
                      >>= \case
                        [] -> pure ()
                        _ -> throwError $ AlreadyExists aid'
                throwError $ Impossible "update article failed"
        DeleteArticle articleId -> do
          deleteOneRow
            articleSchema
            ( \a ->
                ArticleRel8.slug a ==. lit articleId &&. ArticleRel8.author a ==. lit authUserId
            )
            Prelude.id
            >>= \case
              Just _ -> pure ()
              Nothing -> do
                a <- checkArticleId articleId
                send (SqlSelect $ select $ getAuthorForArticle $ lit a) >>= \case
                  [] -> throwError $ Impossible $ "got no author for article id: " <> show articleId
                  [_] -> pure ()
                  _ -> throwError $ Impossible $ "got more than one author for article id: " <> show articleId
                when (ArticleRel8.author a == authUserId) $ throwError $ Forbidden @'D articleId
                throwError $ Impossible "delete article failed"
        AddCommentToArticle articleId (CommentCreate txt) ->
          -- FIXME is th current user following himself??
          mkComment . (,user,True) <$> do
            cid <- CommentId <$> R.ask @UUID
            insertOneRow
              commentSchema
              ( CommentRel8
                  (lit cid)
                  (lit txt)
                  (lit authUserId)
                  (lit articleId)
                  unsafeDefault
                  unsafeDefault
              )
              Prelude.id
              $ do
                send (SqlSelect . select $ getArticleById $ lit articleId) >>= \case
                  [] -> throwError $ NotFound articleId
                  [_] -> pure ()
                  _ -> throwError $ Impossible $ "got more than one article form article id " <> show articleId
                send (SqlSelect . select $ getCommentById $ lit cid) >>= \case
                  [] -> pure ()
                  [_] -> throwError $ Impossible $ "uuid collision" <> show cid
                  _ -> throwError $ Impossible $ "got more than one comment form comment id " <> show cid
                throwError $ Impossible "insert comment failed"
        DeleteComment articleId commentId ->
          deleteOneRow
            commentSchema
            ( \cm ->
                lit commentId ==. CommentRel8.id cm
                  &&. lit articleId ==. CommentRel8.article cm
                  &&. lit authUserId ==. CommentRel8.author cm
            )
            Prelude.id
            >>= \case
              Just _ -> pure ()
              Nothing -> do
                c <- checkCommentId commentId
                a <- checkArticleId articleId
                -- FIXME more specialized error type? 404?
                when (CommentRel8.article c == ArticleRel8.slug a) $ throwError @Text "Comment not belongs to article"
                when (CommentRel8.author c == authUserId) $ throwError $ Forbidden @'D commentId
                throwError $ Impossible "delete comment failed"
        FavoriteArticle (lit -> articleId) ->
          toggleOn
            userFavoriteArticleSchema
            (UserFavoriteArticleRel8 (lit authUserId) articleId unsafeDefault)
            $ getArticleWithAuthorProfileById articleId
        UnfavoriteArticle (lit -> articleId) -> do
          toggleOff
            userFavoriteArticleSchema
            ( \r ->
                UserFavoriteArticleRel8.favoriting r ==. articleId
                  &&. UserFavoriteArticleRel8.favoritedBy r ==. lit authUserId
            )
          getArticleWithAuthorProfileById articleId
  alg hdl (R other) ctx = UserActionInRel8C $ alg (runUserActionInRel8 . hdl) other ctx
  {-# INLINE alg #-}
