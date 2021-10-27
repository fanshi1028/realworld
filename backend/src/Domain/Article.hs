{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Representations for article
--
-- @since 0.1.0.0
module Article where

import Data.Aeson (ToJSON (toEncoding, toJSON), Value (Object))
import qualified Data.HashMap.Strict as HM
import Domain (Domain (Article))
import Field.Tag (Tag)
import GHC.TypeLits (Symbol)
import Storage.Map (ContentOf (..), toArticleId)
import User (UserR)
import Util.JSON.To (Out, multiWrappedWithCountToEncoding, multiWrappedWithCountToJSON, wrappedToEncoding, wrappedToJSON)

-- | Type family for different representations of articles
--
-- @since 0.1.0.0
data family ArticleR (r :: Symbol)

-- | Representation for output
--
-- @since 0.2.0.0
data instance ArticleR "withAuthorProfile" = ArticleWithAuthorProfile
  { article :: ContentOf 'Article,
    tagList :: [Tag], -- ["dragons", "training"],
    favorited :: Bool, -- false,
    favoritesCount :: Natural, -- 0,
    author :: UserR "profile"
  }
  deriving (Eq, Show, Generic)

-- | @since 0.2.0.0
instance ToJSON (ArticleR "withAuthorProfile") where
  toJSON (ArticleWithAuthorProfile a tags b n ur) = case toJSON a of
    Object hm ->
      Object
        . HM.insert "slug" (toJSON $ toArticleId a)
        . HM.insert "tagList" (toJSON tags)
        . HM.insert "favorited" (toJSON b)
        . HM.insert "favoritesCount" (toJSON n)
        . HM.insert "author" (toJSON ur)
        $ hm
    _ -> error "impossible in ToJSON (ArticleR \"withAuthorProfile\")"

-- | @since 0.2.0.0
instance ToJSON (Out (ArticleR "withAuthorProfile")) where
  toJSON = wrappedToJSON "article"
  toEncoding = wrappedToEncoding "article"

-- | @since 0.2.0.0
instance (Foldable t, ToJSON (t (ArticleR "withAuthorProfile"))) => ToJSON (Out (t (ArticleR "withAuthorProfile"))) where
  toJSON = multiWrappedWithCountToJSON "articles" "articlesCount"
  toEncoding = multiWrappedWithCountToEncoding "articles" "articlesCount"
