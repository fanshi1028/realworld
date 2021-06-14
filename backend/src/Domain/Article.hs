{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Domain.Article where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), Value (Array), defaultOptions, genericParseJSON, withObject)
import Data.Aeson.Types (Value (Object))
import Data.Generic.HKD (construct)
import Data.Generics.Labels ()
import Data.UUID (UUID)
import Domain.User (UserR)
import Domain.Util (Body (..), Description, In, Out (..), Slug, Tag, Time, Title, insert', multiWrappedWithCountToEncoding, wrappedParseJSON, wrappedToEncoding)
import GHC.TypeLits (Symbol)
import Validation.Adaptor (WithUpdate, WithValidation)

data family ArticleR (r :: Symbol)

newtype instance ArticleR "id" = ArticleId UUID

-- | Articles
data instance ArticleR "all" = Article
  { slug :: Slug, -- "how-to-train-your-dragon",
    title :: Title, -- "How to train your dragon",
    description :: Description, -- "Ever wonder how?",
    body :: Body, -- "It takes a Jacobian",
    tagList :: [Tag], -- ["dragons", "training"],
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:48:35.824Z",
    favorited :: Bool, -- false,
    favoritesCount :: Natural, -- 0,
    author :: UserR "id"
  }
  deriving (Generic)

--------------------------
--                 m    --
--  mmm   m   m  mm#mm  --
-- #" "#  #   #    #    --
-- #   #  #   #    #    --
-- "#m#"  "mm"#    "mm  --
--------------------------

data instance ArticleR "withAuthorProfile" = ArticleWithAuthorProfile
  { slug :: Slug, -- "how-to-train-your-dragon",
    title :: Title, -- "How to train your dragon",
    description :: Description, -- "Ever wonder how?",
    body :: Body, -- "It takes a Jacobian",
    tagList :: [Tag], -- ["dragons", "training"],
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:48:35.824Z",
    favorited :: Bool, -- false,
    favoritesCount :: Natural, -- 0,
    author :: UserR "profile"
  }
  deriving (Generic, ToJSON)

-- >>> import Domain.Util
-- >>> import Domain.User
-- >>> import Data.Time
-- >>> import Data.Aeson
-- >>> slug = Slug "how-to-train-your-dragon"
-- >>> title = Title "How to train your dragon"
-- >>> description = Description "Ever wonder how?"
-- >>> body = Body "It takes a Jacobian"
-- >>> tags = Tag <$> ["dragon", "training"]
-- >>> timeHelper day = UTCTime (toEnum day) (secondsToDiffTime 3600)
-- >>> createdAt = timeHelper 0
-- >>> updatedAt = timeHelper 10000
-- >>> profile = UserProfile (Email "jake@jake.jake") "jake" (Just $ Bio "I work at statefarm") (Just $ Image "https://static.productionready.io/images/smiley-cyrus.jpg") False
-- >>> article = ArticleWithAuthorProfile slug title description body tags createdAt updatedAt True 999 profile
-- >>> encode article
-- >>> encode $ Out article
-- >>> encode [ article, article ]
-- >>> encode $ Out [ article, article ]
instance ToJSON (Out (ArticleR "withAuthorProfile")) where
  toEncoding = wrappedToEncoding "article"

instance (Foldable t, ToJSON (t (ArticleR "withAuthorProfile"))) => ToJSON (Out (t (ArticleR "withAuthorProfile"))) where
  toEncoding (Out as) = multiWrappedWithCountToEncoding "articles" "articlesCount" as

-------------------
--   "           --
-- mmm    m mm   --
--   #    #"  #  --
--   #    #   #  --
-- mm#mm  #   #  --
-------------------

data instance ArticleR "create" = ArticleCreate
  { title :: Title, -- "How to train your dragon",
    description :: Description, -- "Ever wonder how?",
    body :: Body, -- "You have to believe",
    tagList :: [Tag] -- ["reactjs", "angularjs", "dragons"]
  }
  deriving (Generic, Show)

-- >>> import Data.Aeson
-- >>> eitherDecode @(WithValidation (ArticleR "create")) "{\"title\":\"\", \"description\": \"fjowfewe\"}"
-- >>> eitherDecode @(WithValidation (ArticleR "create")) "{\"title\":\"fjowefjew\", \"description\": \"fjowfewe\"}"
-- >>> eitherDecode @(WithValidation (ArticleR "create")) "{\"title\":\"fjowefjew\", \"description\": \"fjowfewe\", \"body\": null}"
-- >>> eitherDecode @(WithValidation (ArticleR "create")) "{\"title\":\"fjowefjew\", \"description\": \"fjowfewe\", \"body\": \"hwjowf\"}"
-- >>> eitherDecode @(In (WithValidation (ArticleR "create"))) "{\"title\":\"fjowefjew\", \"description\": \"fjowfewe\", \"body\": \"hwjowf\"}"
-- >>> eitherDecode @(In (WithValidation (ArticleR "create"))) "\"hi\""
-- >>> eitherDecode @(In (WithValidation (ArticleR "create"))) "{\"article\":\"hi\"}"
-- Right (Failure ("null title" :| []))
-- Right (Success (ArticleCreate {title = "fjowefjew", description = "fjowfewe", body = "", tagList = []}))
-- Left "Error in $.body: parsing Text failed, expected String, but encountered Null"
-- Right (Success (ArticleCreate {title = "fjowefjew", description = "fjowfewe", body = "hwjowf", tagList = []}))
-- Left "Error in $: key \"article\" not found"
-- Left "Error in $: parsing CreateArticle failed, expected Object, but encountered String"
-- Left "Error in $.article: parsing CreateArticle failed, expected Object, but encountered String"
instance FromJSON (WithValidation (ArticleR "create")) where
  parseJSON =
    withObject "CreateArticle" $
      \( Object
           . insert' "tagList" (Array mempty)
           . insert' "body" "" ->
           o
         ) ->
          construct <$> genericParseJSON defaultOptions o

instance FromJSON (In (WithValidation (ArticleR "create"))) where
  parseJSON = wrappedParseJSON "ArticleCreate" "article"

data instance ArticleR "update" = ArticleUpdate
  { title :: Title, -- "How to train your dragon",
    description :: Description, -- "Ever wonder how?",
    body :: Body -- "You have to believe",
  }
  deriving (Generic, Show)

instance FromJSON (WithValidation (ArticleR "update")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

instance FromJSON (WithUpdate (ArticleR "update"))

instance FromJSON (In (WithUpdate (ArticleR "update"))) where
  parseJSON = wrappedParseJSON "ArticleUpdate" "article"
