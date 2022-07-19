{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Rel8
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Query Helpers
--
-- @since 0.4.0.0
module InRel8.Storage where

import Data.Authentication.HasAuth (AuthOf (..))
import Data.Domain (Domain (Article, Comment, User))
import Data.Domain.Article (ArticleWithAuthorProfile (ArticleWithAuthorProfile))
import Data.Domain.Comment (CommentWithAuthorProfile (CommentWithAuthorProfile))
import Data.Domain.User (UserProfile (UserProfile))
import Data.Field.Tag (Tag)
import Data.Storage.Map.HasStorage (ContentOf (ArticleContent), IdOf (UserId), toUserId)
import InRel8.Storage.Internal.Field ()
import InRel8.Storage.Schema.Article as Article (ArticleRel8 (ArticleRel8, createdAt, slug), articleSchema, author)
import InRel8.Storage.Schema.ArticleHasTag as AHT (ArticleHasTagRel8 (ArticleHasTagRel8), articleHasTagSchema)
import InRel8.Storage.Schema.Comment as Comment (CommentRel8 (CommentRel8, article, id), author, commentSchema)
import InRel8.Storage.Schema.Tag as Tag (TagRel8 (tag), tagSchema)
import InRel8.Storage.Schema.User as User (UserRel8 (UserRel8), userSchema, username)
import InRel8.Storage.Schema.UserFavoriteArticle (UserFavoriteArticleRel8 (UserFavoriteArticleRel8), userFavoriteArticleSchema)
import InRel8.Storage.Schema.UserFollowUser as UFU (UserFollowUserRel8 (UserFollowUserRel8), userFollowUserSchema)
import Rel8 (Expr, ListTable, Query, Result, asc, countRows, each, exists, filter, lit, many, orderBy, where_, (&&.), (==.))

-- * Basic Query

-- ** User

-- | @since 0.4.0.0
getUserById :: Expr (IdOf 'User) -> Query (UserRel8 Expr)
getUserById uid = do
  each userSchema >>= Rel8.filter ((uid ==.) . User.username)

-- ** Article

-- | @since 0.4.0.0
getArticleById :: Expr (IdOf 'Article) -> Query (ArticleRel8 Expr)
getArticleById aid = do
  each articleSchema >>= Rel8.filter ((aid ==.) . slug)

-- | @since 0.4.0.0
getAuthorForArticle :: ArticleRel8 Expr -> Query (UserRel8 Expr)
getAuthorForArticle a = do
  each userSchema >>= Rel8.filter ((Article.author a ==.) . User.username)

-- ** Comment

-- | @since 0.4.0.0
getCommentById :: Expr (IdOf 'Comment) -> Query (CommentRel8 Expr)
getCommentById cid = do
  each commentSchema >>= Rel8.filter ((cid ==.) . Comment.id)

-- | @since 0.4.0.0
getAuthorForComment :: CommentRel8 Expr -> Query (UserRel8 Expr)
getAuthorForComment c = do
  each userSchema >>= Rel8.filter ((Comment.author c ==.) . User.username)

-- | @since 0.4.0.0
getArticleForComment :: CommentRel8 Expr -> Query (ArticleRel8 Expr)
getArticleForComment c = do
  each articleSchema >>= Rel8.filter ((Comment.article c ==.) . slug)

-- ** Tag

-- | @since 0.4.0.0
getTagById :: Expr Tag -> Query (TagRel8 Expr)
getTagById tid = each tagSchema >>= Rel8.filter ((tid ==.) . Tag.tag)

-- ** Article Has Tag

-- | @since 0.4.0.0
filterArticleHasTag :: Expr (IdOf 'Article) -> Expr Tag -> Query ()
filterArticleHasTag aid tid = do
  (ArticleHasTagRel8 aid' tid' _) <- each articleHasTagSchema
  where_ (tid ==. tid' &&. aid ==. aid')

-- | @since 0.4.0.0
getTagForArticle :: ArticleRel8 Expr -> Query (TagRel8 Expr)
getTagForArticle (Article.slug -> aid) = do
  t@(Tag.tag -> tid) <- each tagSchema
  filterArticleHasTag aid tid
  pure t

-- | @since 0.4.0.0
getArticleForTag :: TagRel8 Expr -> Query (ArticleRel8 Expr)
getArticleForTag (Tag.tag -> tid) = do
  a@(Article.slug -> aid) <- each articleSchema
  filterArticleHasTag aid tid
  pure a

-- ** User Follow User

-- | @since 0.4.0.0
filterUserFollowUser :: Expr (IdOf 'User) -> Expr (IdOf 'User) -> Query ()
filterUserFollowUser uidFol uidFolBy = do
  (UserFollowUserRel8 uidFol' uidFolBy' _) <- each userFollowUserSchema
  where_ (uidFol ==. uidFol' &&. uidFolBy ==. uidFolBy')

-- | @since 0.4.0.0
getUserFollowedByUser :: UserRel8 Expr -> Query (UserRel8 Expr)
getUserFollowedByUser (User.username -> uidFolBy) = do
  u@(User.username -> uid) <- each userSchema
  filterUserFollowUser uid uidFolBy
  pure u

-- | @since 0.4.0.0
getFollowerForUser :: UserRel8 Expr -> Query (UserRel8 Expr)
getFollowerForUser (User.username -> uid) = do
  u@(User.username -> uidFolBy) <- each userSchema
  filterUserFollowUser uid uidFolBy
  pure u

-- ** User Favorite Article

-- | @since 0.4.0.0
filterUserFavoriteArticle :: Expr (IdOf 'User) -> Expr (IdOf 'Article) -> Query ()
filterUserFavoriteArticle uid aid = do
  (UserFavoriteArticleRel8 uid' aid' _) <- each userFavoriteArticleSchema
  where_ (aid ==. aid' &&. uid ==. uid')

-- | @since 0.4.0.0
getArticleFavoritedByUser :: UserRel8 Expr -> Query (ArticleRel8 Expr)
getArticleFavoritedByUser (User.username -> uid) = do
  u@(Article.slug -> aid) <- each articleSchema
  filterUserFavoriteArticle uid aid
  pure u

-- | @since 0.4.0.0
getUserFavoritingForArticle :: ArticleRel8 Expr -> Query (UserRel8 Expr)
getUserFavoritingForArticle (Article.slug -> aid) = do
  u@(User.username -> uid) <- each userSchema
  filterUserFavoriteArticle uid aid
  pure u

-- * Query helper

-- | @since 0.4.0.0
isFollow :: Maybe (AuthOf 'User) -> UserRel8 Expr -> Query (Expr Bool)
isFollow mAuth u = case mAuth of
  Nothing -> pure $ lit False
  Just (lit . toUserId -> uid) ->
    exists $
      getUserById uid
        >>= getFollowerForUser
        >>= Rel8.filter (on (==.) User.username u)

-- | @since 0.4.0.0
getProfile :: Maybe (AuthOf 'User) -> UserRel8 Expr -> Query (UserRel8 Expr, Expr Bool)
getProfile mAuth u = (u,) <$> isFollow mAuth u

-- | @since 0.4.0.0
getArticles ::
  Maybe (AuthOf 'User) ->
  Query
    ( ArticleRel8 Expr,
      UserRel8 Expr,
      ListTable Expr (Expr Tag),
      Expr Bool,
      Expr Bool,
      Expr Int64
    )
getArticles mAuth = do
  a <- each articleSchema & orderBy (Article.createdAt >$< asc)
  u <- getAuthorForArticle a
  tl <- Rel8.many $ tag <$> getTagForArticle a
  fav <- case mAuth of
    Nothing -> pure $ lit False
    Just (lit . toUserId -> uid) -> exists $ getUserFavoritingForArticle a >>= Rel8.filter (==. uid) . User.username
  favC <- countRows $ getUserFavoritingForArticle a
  (a,u,tl,,fav,favC) <$> isFollow mAuth u

-- * Construct result

-- | @since 0.4.0.0
mkComment :: (CommentRel8 Result, UserRel8 Result, Bool) -> CommentWithAuthorProfile
mkComment (CommentRel8 cid t _ _ ct ut, UserRel8 (UserId un) em _ bio' img _ _, fol) =
  CommentWithAuthorProfile cid ct ut t $ UserProfile (UserAuth em un bio' img) fol

-- | @since 0.4.0.0
mkAuth :: UserRel8 Result -> AuthOf 'User
mkAuth (UserRel8 (UserId un) em _ bio' uImg _ _) = UserAuth em un bio' uImg

-- | @since 0.4.0.0
mkProfile :: (UserRel8 Result, Bool) -> UserProfile
mkProfile (mkAuth -> auth, fol) = UserProfile auth fol

-- | @since 0.4.0.0
mkArticle :: (ArticleRel8 Result, UserRel8 Result, [Tag], Bool, Bool, Int64) -> ArticleWithAuthorProfile
mkArticle (ArticleRel8 _ tt des bd uid ct ut, u, ts, fol, fav, fromIntegral -> favC) =
  ArticleWithAuthorProfile (ArticleContent tt des bd ct ut uid) ts fav favC $ curry mkProfile u fol
