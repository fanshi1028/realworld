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
-- @since 0.3.0.0
module Storage.Map.Internal.HasStorage.Article where

import Data.Aeson (ToJSON)
import Domain (Domain (Article, User))
import Field.Body (Body)
import Field.Description (Description)
import Field.Slug (Slug, titleToSlug)
import Field.Time (Time)
import Field.Title (Title)
import GHC.Records (HasField, getField)
import Servant (FromHttpApiData)
import Storage.Map.Internal.HasStorage (HasStorage (..))
import Storage.Map.Internal.HasStorage.User ()
import Util.Validation (WithValidation)

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
toArticleId :: HasField "title" a Title => a -> IdOf 'Article
toArticleId = ArticleId . titleToSlug . getField @"title"
