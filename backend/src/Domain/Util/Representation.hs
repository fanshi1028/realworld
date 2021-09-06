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
module Domain.Util.Representation (Transform (transform)) where

import qualified Data.Text as Text (intercalate, toLower)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Field (Slug (Slug), Title (..), Username)
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)
import Relude.Extra (un)

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
  transform = ArticleId . Slug . Text.intercalate "-" . words . Text.toLower . un . getField @"title"

-- * Comment

-- | @since 0.2.0.0
instance (HasField "id" (CommentR s) (CommentR "id")) => Transform CommentR s "id" where
  transform = getField @"id"
