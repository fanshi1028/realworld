{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @since 0.2.0.0
module Storage.Map.Internal.HasCreate.Article where

import Data.Aeson (FromJSON (parseJSON), Value (Array, Object), defaultOptions, genericParseJSON, withObject)
import Data.Generic.HKD (construct)
import Field.Body (Body)
import Field.Description (Description)
import Field.Tag (Tag)
import Field.Title (Title)
import Storage.Map.Internal.HasCreate (HasCreate (CreateOf))
import Util.JSON.From (In, insert', wrappedParseJSON)
import Util.Validation (WithValidation)

-- | @since 0.2.0.0
instance HasCreate "article" where
  data CreateOf "article" = ArticleCreate
    { title :: Title, -- "How to train your dragon",
      description :: Description, -- "Ever wonder how?",
      body :: Body, -- "You have to believe",
      tagList :: [Tag] -- ["reactjs", "angularjs", "dragons"]
    }
    deriving (Show, Generic)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (CreateOf "article")) where
  parseJSON =
    withObject "CreateArticle" $
      Object . insert' "tagList" (Array mempty)
        >>> genericParseJSON defaultOptions
        >>> fmap construct

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (CreateOf "article"))) where
  parseJSON = wrappedParseJSON "ArticleCreate" "article"
-- ^
-- >>> import Data.Aeson (eitherDecode')
-- >>> eitherDecode' @(WithValidation (CreateOf "article")) "{\"title\":\"kkfewoiw\", \"description\": \"fjow\"}"
-- >>> eitherDecode' @(WithValidation (CreateOf "article")) "{\"title\":\"kkfewoiw\", \"description\": \"fjow\", \"body\": null}"
-- >>> eitherDecode' @(WithValidation (CreateOf "article")) "{\"title\":\"kkfewoiw\", \"description\": \"fjow\", \"body\": \"hwjowf\"}"
-- >>> eitherDecode' @(In (WithValidation (CreateOf "article"))) "{\"title\":\"kkfjfiw\", \"description\": \"fjow\", \"body\": \"hwjowf\"}"
-- >>> eitherDecode' @(In (WithValidation (CreateOf "article"))) "{\"article\":\"hi\"}"
-- Left "Error in $: parsing Storage.Map.Internal.HasCreate.Article.CreateOf(ArticleCreate) failed, key \"body\" not found"
-- Left "Error in $.body: parsing Text failed, expected String, but encountered Null"
-- Right (Success (ArticleCreate {title = "kkfewoiw", description = "fjow", body = "hwjowf", tagList = []}))
-- Left "Error in $: key \"article\" not found"
-- Left "Error in $.article: parsing CreateArticle failed, expected Object, but encountered String"
