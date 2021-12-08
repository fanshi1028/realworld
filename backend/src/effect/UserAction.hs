{-# LANGUAGE DataKinds #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of users' action
--
-- @since 0.1.0.0
module UserAction where

import Authentication.HasAuth (AuthOf (..))
import Domain (Domain (Article, Comment, User))
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR)
import Storage.Map (CreateOf, IdOf, Patch, UpdateOf)

-- | @since 0.3.0.0
-- Actions that can only be carried out by __authenticated__ users.
data UserActionE (m :: Type -> Type) a where
  -- | @since 0.1.0.0
  -- Get the info of the current authenticated user.
  GetCurrentUser :: UserActionE m (UserR "authWithToken")
  -- | @since 0.3.0.0
  -- Update the profile of the current authenticated user.
  UpdateUser :: Patch (UpdateOf 'User) -> UserActionE m (AuthOf 'User)
  -- | @since 0.1.0.0
  -- Follow the user specified by the id.
  FollowUser :: IdOf 'User -> UserActionE m (UserR "profile")
  -- | @since 0.1.0.0
  -- Unfollow the user specified by the id.
  UnfollowUser :: IdOf 'User -> UserActionE m (UserR "profile")
  -- | @since 0.1.0.0
  -- Create the article specified by the id.
  CreateArticle :: CreateOf 'Article -> UserActionE m (ArticleR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Update the authenticated user's own article specified by the id.
  UpdateArticle :: IdOf 'Article -> Patch (UpdateOf 'Article) -> UserActionE m (ArticleR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Delete the authenticated user's own article specified by the id.
  DeleteArticle :: IdOf 'Article -> UserActionE m ()
  -- | @since 0.1.0.0
  -- Leave a comment to the article specified by the id.
  AddCommentToArticle :: IdOf 'Article -> CreateOf 'Comment -> UserActionE m (CommentR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Delete the authenticated user's own comment specified by the id from the article.
  DeleteComment :: IdOf 'Article -> IdOf 'Comment -> UserActionE m ()
  -- | @since 0.1.0.0
  -- Favorite the aritcle specified by the id.
  FavoriteArticle :: IdOf 'Article -> UserActionE m (ArticleR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Unfavorite the aritcle specified by the id.
  UnfavoriteArticle :: IdOf 'Article -> UserActionE m (ArticleR "withAuthorProfile")
  -- | @since 0.1.0.0
  -- Get articles feed recommendated for the authenticated user.
  FeedArticles :: UserActionE m [ArticleR "withAuthorProfile"]
