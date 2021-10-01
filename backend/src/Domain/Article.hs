{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Representations for article
--
-- @since 0.1.0.0
module Domain.Article (ArticleR (..)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value (Array), defaultOptions, genericParseJSON, withObject, (.!=), (.:?))
import Data.Aeson.Types (Value (Object))
import Data.Generic.HKD (HKD, build, construct)
import qualified Data.HashMap.Strict as HM (insert)
import qualified Data.Semigroup as SG (Last)
import Domain.User (UserR)
import Domain.Util.Field (Body, Description, Slug (Slug), Tag, Time, Title, titleToSlug)
import Domain.Util.JSON.From (In, insert', wrappedParseJSON)
import Domain.Util.JSON.To (Out, multiWrappedWithCountToEncoding, multiWrappedWithCountToJSON, wrappedToEncoding, wrappedToJSON)
import Domain.Util.Validation (WithValidation)
import GHC.Records (getField)
import GHC.TypeLits (Symbol)
import Servant (FromHttpApiData)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | Type family for different representations of articles
--
-- @since 0.1.0.0
data family ArticleR (r :: Symbol)

-- | Id which can be used to uniquely idenitify an article.
--
-- @since 0.1.0.0
newtype instance ArticleR "id" = ArticleId Slug
  deriving (Show, Eq)
  deriving newtype (Hashable, ToJSON)

-- | @since 0.1.0.0
deriving via (WithValidation Slug) instance FromHttpApiData (WithValidation (ArticleR "id"))

-- | Representation in storage
--
-- @since 0.1.0.0
data instance ArticleR "all" = Article
  { title :: Title, -- "How to train your dragon",
    description :: Description, -- "Ever wonder how?",
    body :: Body, -- "It takes a Jacobian",
    -- tagList :: [Tag], -- ["dragons", "training"],
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:48:35.824Z",
    -- favorited :: Bool, -- false,
    -- favoritesCount :: Natural, -- 0,
    author :: UserR "id"
  }
  deriving (Show, Eq, Generic, ToJSON)

--------------------------
--                 m    --
--  mmm   m   m  mm#mm  --
-- #" "#  #   #    #    --
-- #   #  #   #    #    --
-- "#m#"  "mm"#    "mm  --
--------------------------

-- | Representation for output
--
-- @since 0.2.0.0
data instance ArticleR "withAuthorProfile" = ArticleWithAuthorProfile
  { article :: ArticleR "all",
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
        . HM.insert "slug" (toJSON $ titleToSlug $ getField @"title" a)
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

-------------------
--   "           --
-- mmm    m mm   --
--   #    #"  #  --
--   #    #   #  --
-- mm#mm  #   #  --
-------------------

-- | Representation for creation
--
-- @since 0.1.0.0
data instance ArticleR "create" = ArticleCreate
  { title :: Title, -- "How to train your dragon",
    description :: Description, -- "Ever wonder how?",
    body :: Body, -- "You have to believe",
    tagList :: [Tag] -- ["reactjs", "angularjs", "dragons"]
  }
  deriving (Eq, Show, Generic)

-- | @since 0.1.0.0
instance FromJSON (WithValidation (ArticleR "create")) where
  parseJSON =
    withObject "CreateArticle" $
      \(Object . insert' "tagList" (Array mempty) -> o) -> construct <$> genericParseJSON defaultOptions o
-- ^
-- >>> eitherDecode' @(WithValidation (ArticleR "create")) "{\"title\":\"fjowefjew\", \"description\": \"fjowfewe\"}"
-- >>> eitherDecode' @(WithValidation (ArticleR "create")) "{\"title\":\"fjowefjew\", \"description\": \"fjowfewe\", \"body\": null}"
-- >>> eitherDecode' @(WithValidation (ArticleR "create")) "{\"title\":\"fjowefjew\", \"description\": \"fjowfewe\", \"body\": \"hwjowf\"}"
-- >>> eitherDecode' @(In (WithValidation (ArticleR "create"))) "{\"title\":\"fjowefjew\", \"description\": \"fjowfewe\", \"body\": \"hwjowf\"}"
-- >>> eitherDecode' @(In (WithValidation (ArticleR "create"))) "{\"article\":\"hi\"}"
-- Left "Error in $: parsing Domain.Article.ArticleR(ArticleCreate) failed, key \"body\" not found"
-- Left "Error in $.body: parsing Text failed, expected String, but encountered Null"
-- Right (Success (ArticleCreate {title = "fjowefjew", description = "fjowfewe", body = "hwjowf", tagList = []}))
-- Left "Error in $: key \"article\" not found"
-- Left "Error in $.article: parsing CreateArticle failed, expected Object, but encountered String"

-- | @since 0.1.0.0
instance FromJSON (In (WithValidation (ArticleR "create"))) where
  parseJSON = wrappedParseJSON "ArticleCreate" "article"

-- | Representation for update
--
-- @since 0.1.0.0
newtype instance ArticleR "update" = ArticleUpdate (HKD (HKD (ArticleR "all") SG.Last) Maybe) deriving (Generic)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (ArticleR "update")) where
  parseJSON = withObject "UpdateArticle" $ \o ->
    ArticleUpdate
      <<$>> construct
        <$> construct
          ( build @(HKD (HKD (HKD (ArticleR "all") SG.Last) Maybe) WithValidation)
              (o .:? "title" .!= pure Nothing)
              (o .:? "description" .!= pure Nothing)
              (o .:? "body" .!= pure Nothing)
              (pure $ pure Nothing)
              (pure $ pure Nothing)
              (pure $ pure Nothing)
          )

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (ArticleR "update"))) where
  parseJSON = wrappedParseJSON "ArticleUpdate" "article"
