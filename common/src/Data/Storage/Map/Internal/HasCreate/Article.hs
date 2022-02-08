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
-- Create 'Article' in storage
--
-- @since 0.4.0.0
module Data.Storage.Map.Internal.HasCreate.Article where

import Data.Aeson (FromJSON (parseJSON), Value (Array, Object), defaultOptions, genericParseJSON, withObject)
import Data.Domain (Domain (Article))
import Data.Field.Body (Body)
import Data.Field.Description (Description)
import Data.Field.Tag (Tag)
import Data.Field.Title (Title)
import Data.Generic.HKD (construct)
import Data.Storage.Map.Internal.HasCreate (HasCreate (CreateOf))
import Data.Util.JSON.From (In, insert', wrappedParseJSON)
import Data.Util.Validation (WithValidation)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.3.0.0
instance HasCreate 'Article where
  data CreateOf 'Article = ArticleCreate
    { title :: Title, -- "How to train your dragon",
      description :: Description, -- "Ever wonder how?",
      body :: Body, -- "You have to believe",
      tagList :: [Tag] -- ["reactjs", "angularjs", "dragons"]
    }
    deriving (Show, Generic)

-- | @since 0.3.0.0
instance FromJSON (WithValidation (CreateOf 'Article)) where
  parseJSON =
    withObject "CreateArticle" $
      Object . insert' "tagList" (Array mempty)
        >>> genericParseJSON defaultOptions
        >>> fmap construct
-- ^
-- ==== Success
-- >>> eitherDecode' @(WithValidation (CreateOf 'Article)) "{\"title\":\"kkfewoiw\", \"description\": \"fjow\", \"body\": \"hwjowf\"}"
-- Right (Success (ArticleCreate {title = "kkfewoiw", description = "fjow", body = "hwjowf", tagList = []}))
--
-- ==== Fail
-- >>> eitherDecode' @(WithValidation (CreateOf 'Article)) "{\"title\":\"kkfewoiw\", \"description\": \"fjow\"}"
-- Left "Error in $: parsing Storage.Map.Internal.HasCreate.Article.CreateOf(ArticleCreate) failed, key \"body\" not found"
--
-- >>> eitherDecode' @(WithValidation (CreateOf 'Article)) "{\"title\":\"kkfewoiw\", \"description\": \"fjow\", \"body\": null}"
-- Left "Error in $.body: parsing Text failed, expected String, but encountered Null"

-- | @since 0.3.0.0
instance FromJSON (In (WithValidation (CreateOf 'Article))) where
  parseJSON = wrappedParseJSON "ArticleCreate" "article"
-- ^
-- ==== Success
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'Article))) "{\"article\": {\"title\":\"kkfewoiw\", \"description\": \"fjow\", \"body\": \"hwjowf\"}}"
-- Right (In (Success (ArticleCreate {title = "kkfewoiw", description = "fjow", body = "hwjowf", tagList = []})))
--
-- ==== Fail
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'Article))) "{\"title\":\"kkfjfiw\", \"description\": \"fjow\", \"body\": \"hwjowf\"}"
-- Left "Error in $: key \"article\" not found"
--
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'Article))) "{\"article\":\"hi\"}"
-- Left "Error in $.article: parsing CreateArticle failed, expected Object, but encountered String"
