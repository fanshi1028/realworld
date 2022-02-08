{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Storage for 'Article'
--
-- @since 0.4.0.0
module Data.Storage.Map.Internal.HasStorage.Article where

import Data.Aeson (ToJSON)
import Data.Domain (Domain (Article, User))
import Data.Field.Body (Body)
import Data.Field.Description (Description)
import Data.Field.Slug (Slug, titleToSlug)
import Data.Field.Time (Time)
import Data.Field.Title (Title)
import Data.Generics.Product (HasField', getField)
import Data.Storage.Map.Internal.HasStorage (HasStorage (..))
import Data.Storage.Map.Internal.HasStorage.User ()
import Data.Util.Validation (WithValidation)
import Servant (FromHttpApiData)

-- | @since 0.3.0.0
instance HasStorage 'Article where
  newtype IdOf 'Article = ArticleId Slug deriving (Eq, Show, Hashable, ToJSON)
  data ContentOf 'Article = ArticleContent
    { title :: Title, -- "How to train your dragon",
      description :: Description, -- "Ever wonder how?",
      body :: Body, -- "It takes a Jacobian",
      -- tagList :: [Tag], -- ["dragons", "training"],
      createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
      updatedAt :: Time, -- "2016-02-18T03:48:35.824Z",
      -- favorited :: Bool, -- false,
      -- favoritesCount :: Natural, -- 0,
      author :: IdOf 'User
    }
    deriving (Show, Eq, Generic)

-- | @since 0.3.0.0
instance Ord (ContentOf 'Article) where
  (<=) = (<=) `on` Down . createdAt

-- | @since 0.3.0.0
deriving via (WithValidation Slug) instance FromHttpApiData (WithValidation (IdOf 'Article))

-- | @since 0.3.0.0
-- FIXME tojson have slug
instance ToJSON (ContentOf 'Article)

-- * Helper

-- | @since 0.3.0.0
toArticleId :: (HasField' "title" a Title) => a -> IdOf 'Article
toArticleId = ArticleId . titleToSlug . getField @"title"
