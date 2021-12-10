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
-- @since 0.2.0.0
module Domain.Article where

import Data.Aeson (ToJSON (toEncoding, toJSON), Value (Object))
import qualified Data.HashMap.Strict as HM
import Domain (Domain (Article))
import Domain.User (UserR)
import Field.Tag (Tag)
import GHC.Records (getField)
import GHC.TypeLits (Symbol)
import Storage.Map (ContentOf (..), toArticleId)
import Util.JSON.To (Out, multiWrappedWithCountToEncoding, multiWrappedWithCountToJSON, wrappedToEncoding, wrappedToJSON)

-- $setup
-- >>> import Data.Aeson (encode)
-- >>> import Field.Title (Title (Title))
-- >>> import Field.Description (Description (Description))
-- >>> import Field.Body (Body (Body))
-- >>> import Field.Username (Username (Username))
-- >>> import Field.Email (Email (Email))
-- >>> import Field.Bio (Bio (Bio))
-- >>> import Field.Tag (Tag (Tag))
-- >>> import Field.Image (Image (Image))
-- >>> import Storage.Map (IdOf (UserId))
-- >>> import Authentication (AuthOf (UserAuth))
-- >>> import Domain.User (UserR (UserProfile))
-- >>> import Data.Time (UTCTime (UTCTime))
-- >>> import Util.JSON.To (Out (Out))
-- >>> t = UTCTime (toEnum 0) (toEnum 0)
-- >>> exampleArticle = ArticleContent (Title "How to train your dragon") (Description "Ever wonder how?") (Body "It takes a Jacobian") t t (UserId $ Username "jake")
-- >>> exampleProfile = UserProfile (UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")) False
-- >>> example = ArticleWithAuthorProfile exampleArticle [Tag "dragons", Tag "training"] False 0 exampleProfile

-- | @since 0.2.0.0
-- Type family for different representations of articles
data family ArticleR (r :: Symbol)

-- | @since 0.2.0.0
-- >>> example
-- ArticleWithAuthorProfile {article = ArticleContent {title = "How to train your dragon", description = "Ever wonder how?", body = "It takes a Jacobian", createdAt = 1858-11-17 00:00:00 UTC, updatedAt = 1858-11-17 00:00:00 UTC, author = UserId "jake"}, tagList = ["dragons","training"], favorited = False, favoritesCount = 0, author = UserProfile {profile = UserAuth {email = "jake@jake.jake", username = "jake", bio = "I work at statefarm", image = "https://static.productionready.io/images/smiley-cyrus.jpg"}, following = False}}
data instance ArticleR "withAuthorProfile" = ArticleWithAuthorProfile
  { article :: ContentOf 'Article,
    tagList :: [Tag],
    favorited :: Bool,
    favoritesCount :: Natural,
    author :: UserR "profile"
  }
  deriving (Eq, Show, Generic)

-- | @since 0.3.0.0
instance Ord (ArticleR "withAuthorProfile") where
  (<=) = (<=) `on` getField @"article"

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
-- ^
-- >>> encode example
-- "{\"title\":\"How to train your dragon\",\"author\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"},\"favorited\":false,\"tagList\":[\"dragons\",\"training\"],\"body\":\"It takes a Jacobian\",\"slug\":\"how-to-train-your-dragon\",\"favoritesCount\":0,\"createdAt\":\"1858-11-17T00:00:00Z\",\"updatedAt\":\"1858-11-17T00:00:00Z\",\"description\":\"Ever wonder how?\"}"

-- | @since 0.2.0.0
instance ToJSON (Out (ArticleR "withAuthorProfile")) where
  toJSON = wrappedToJSON "article"
  toEncoding = wrappedToEncoding "article"
-- ^
-- >>> encode $ Out example
-- "{\"article\":{\"title\":\"How to train your dragon\",\"author\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"},\"favorited\":false,\"tagList\":[\"dragons\",\"training\"],\"body\":\"It takes a Jacobian\",\"slug\":\"how-to-train-your-dragon\",\"favoritesCount\":0,\"createdAt\":\"1858-11-17T00:00:00Z\",\"updatedAt\":\"1858-11-17T00:00:00Z\",\"description\":\"Ever wonder how?\"}}"

-- | @since 0.2.0.0
instance (Foldable t, ToJSON (t (ArticleR "withAuthorProfile"))) => ToJSON (Out (t (ArticleR "withAuthorProfile"))) where
  toJSON = multiWrappedWithCountToJSON "articles" "articlesCount"
  toEncoding = multiWrappedWithCountToEncoding "articles" "articlesCount"
-- ^
-- >>> encode $ Out [ example ]
-- "{\"articles\":[{\"title\":\"How to train your dragon\",\"author\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"},\"favorited\":false,\"tagList\":[\"dragons\",\"training\"],\"body\":\"It takes a Jacobian\",\"slug\":\"how-to-train-your-dragon\",\"favoritesCount\":0,\"createdAt\":\"1858-11-17T00:00:00Z\",\"updatedAt\":\"1858-11-17T00:00:00Z\",\"description\":\"Ever wonder how?\"}],\"articlesCount\":1}"
