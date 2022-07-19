{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Updating in storage for 'Article'
--
-- @since 0.4.0.0
module Data.Storage.Map.HasUpdate.Internal.Article where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))
import Data.Domain (Domain (Article))
import Data.Field.Body (Body)
import Data.Field.Description (Description)
import Data.Field.Title (Title)
import Data.Generic.HKD (HKD, build, construct)
import Data.Generics.Product (HasField' (field'))
import Data.Generics.Product.Fields (getField)
import qualified Data.Semigroup as SG
import Data.Storage.Map.HasStorage.Internal (ContentOf)
import Data.Storage.Map.HasStorage.Internal.Article ()
import Data.Storage.Map.HasUpdate.Internal (HasUpdate (UpdateOf), Patch, updatableKeys)
import Data.Util.JSON.From (In, wrappedParseJSON)
import Data.Util.Validation (WithValidation)
import Relude.Extra ((.~))

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.3.0.0
instance HasUpdate 'Article where
  data UpdateOf 'Article = ArticleUpdate
    { title :: Title,
      description :: Description,
      body :: Body
    }
    deriving (Generic)

-- | @since 0.3.0.0
instance FromJSON (WithValidation (Patch (UpdateOf 'Article))) where
  parseJSON = withObject "UpdateArticle" $ \o -> do
    updatableKeys ["title", "description", "body"] o
    construct
      <$> construct
        ( build @(HKD (HKD (HKD (UpdateOf 'Article) SG.Last) Maybe) WithValidation)
            (o .:? "title" .!= pure Nothing)
            (o .:? "description" .!= pure Nothing)
            (o .:? "body" .!= pure Nothing)
        )
-- ^
-- ==== Success
-- >>> eitherDecode' @(WithValidation (Patch (UpdateOf 'Article))) "{\"title\":\"ewofjowejf\"}"
-- Right (Success ArticleUpdate {title = Just (Last {getLast = "ewofjowejf"}), description = Nothing, body = Nothing})
--
-- ==== Fail
-- >>> eitherDecode' @(WithValidation (Patch (UpdateOf 'Article))) "{\"notUpdatable\":\"hi\"}"
-- Left "Error in $: Only keys [\"title\",\"description\",\"body\"] are updatable, while we found other keys: [\"notUpdatable\"]"

-- | @since 0.3.0.0
instance FromJSON (In (WithValidation (Patch (UpdateOf 'Article)))) where
  parseJSON = wrappedParseJSON "ArticleUpdate" "article"
-- ^
-- ==== Success
-- >>> eitherDecode' @(In (WithValidation (Patch (UpdateOf 'Article)))) "{\"article\":{\"title\":\"ewofjowejf\"}}"
-- Right (In (Success ArticleUpdate {title = Just (Last {getLast = "ewofjowejf"}), description = Nothing, body = Nothing}))
--
-- ==== Fail
-- >>> eitherDecode' @(In (WithValidation (Patch (UpdateOf 'Article)))) "{\"title\":\"ewofjowejf\"}"
-- Left "Error in $: key \"article\" not found"
--
-- >>> eitherDecode' @(In (WithValidation (Patch (UpdateOf 'Article)))) "{\"article\":{\"notUpdatable\":\"hi\"}}"
-- Left "Error in $.article: Only keys [\"title\",\"description\",\"body\"] are updatable, while we found other keys: [\"notUpdatable\"]"

-- * Helper

-- | @since 0.3.0.0
toArticlePatch :: Patch (UpdateOf 'Article) -> Patch (ContentOf 'Article)
toArticlePatch update =
  mempty @(Patch (ContentOf 'Article))
    & field' @"title" .~ getField @"title" update
    & field' @"description" .~ getField @"description" update
    & field' @"body" .~ getField @"body" update
