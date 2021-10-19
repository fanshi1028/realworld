{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Transforms between different representations of Domain Types.
--
-- @since 0.1.0.0
module Util.Representation (Transform (transform)) where

import Article (ArticleR (..))
import Comment (CommentR (..))
import User (UserR (..))
import Util.Field (Title, Username, titleToSlug)
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)

-- | Transform between different representation of the same data
--
-- @since 0.2.0.0
class Transform (r :: Symbol -> Type) (s1 :: Symbol) (s2 :: Symbol) where
  transform :: r s1 -> r s2

-- * User

-- | @since 0.2.0.0
instance (HasField "username" (UserR s) Username) => Transform UserR s "id" where
  transform = UserId . getField @"username"

-- | @since 0.2.0.0
instance Transform UserR "all" "auth" where
  transform (User em _ name bio' img) = UserAuth em name bio' img

-- * Article

-- | @since 0.2.0.0
instance (HasField "title" (ArticleR s) Title) => Transform ArticleR s "id" where
  transform = ArticleId . titleToSlug . getField @"title"

-- * Comment

-- | @since 0.2.0.0
instance (HasField "id" (CommentR s) (CommentR "id")) => Transform CommentR s "id" where
  transform = getField @"id"
