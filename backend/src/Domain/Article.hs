{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Domain.Article (ArticleR (..)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), Value (Array), defaultOptions, genericParseJSON, genericToJSON, withObject)
import Data.Aeson.Encoding (value)
import Data.Aeson.Types (Value (Object))
import Data.Generic.HKD (construct)
import qualified Data.HashMap.Strict as HM
import Domain.User (UserR)
import Domain.Util.Field (Body, Description, Slug (Slug), Tag, Time, Title)
import Domain.Util.JSON.From (In, insert', updatableParseJSON, wrappedParseJSON)
import Domain.Util.JSON.To (Out (Out), multiWrappedWithCountToEncoding, wrapEncoding)
import Domain.Util.Validation (WithUpdate, WithValidation)
import GHC.TypeLits (Symbol)
import Servant (FromHttpApiData (parseUrlPiece))

data family ArticleR (r :: Symbol)

newtype instance ArticleR "id" = ArticleId Slug
  deriving (Show, Eq)
  deriving newtype (Hashable, ToJSON)

-- | Articles
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
  deriving (Generic, ToJSON)

--------------------------
--                 m    --
--  mmm   m   m  mm#mm  --
-- #" "#  #   #    #    --
-- #   #  #   #    #    --
-- "#m#"  "mm"#    "mm  --
--------------------------

-- data instance ArticleR "withAuthorProfile" = ArticleWithAuthorProfile
--   { slug :: Slug, -- "how-to-train-your-dragon",
--     title :: Title, -- "How to train your dragon",
--     description :: Description, -- "Ever wonder how?",
--     body :: Body, -- "It takes a Jacobian",
--     tagList :: [Tag], -- ["dragons", "training"],
--     createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
--     updatedAt :: Time, -- "2016-02-18T03:48:35.824Z",
--     favorited :: Bool, -- false,
--     favoritesCount :: Natural, -- 0,
--     author :: UserR "profile"
--   }
--   deriving (Generic, ToJSON)

data instance ArticleR "withAuthorProfile" = ArticleWithAuthorProfile
  { slug :: ArticleR "id",
    article :: ArticleR "all",
    tagList :: [Tag], -- ["dragons", "training"],
    favorited :: Bool, -- false,
    favoritesCount :: Natural, -- 0,
    author :: UserR "profile"
  }
  deriving (Generic)

instance ToJSON (ArticleR "withAuthorProfile") where
  toEncoding (ArticleWithAuthorProfile aid a tags b n ur) =
    case genericToJSON defaultOptions a of
      Object hm ->
        value
          . Object
          . HM.insert "slug" (toJSON aid)
          . HM.insert "tagList" (toJSON tags)
          . HM.insert "favorited" (toJSON b)
          . HM.insert "favoritesCount" (toJSON n)
          . HM.insert "author" (toJSON ur)
          $ hm
      _ -> error "impossible in ToJSON (ArticleR \"withAuthorProfile\")"

instance ToJSON (Out (ArticleR "withAuthorProfile")) where
  toEncoding (Out a) = wrapEncoding "article" $ toEncoding a

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
-- instance ToJSON (Out (ArticleR "withAuthorProfile")) where
--   toEncoding = wrappedToEncoding "article"

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
      \(Object . insert' "tagList" (Array mempty) -> o) -> construct <$> genericParseJSON defaultOptions o

instance FromJSON (In (WithValidation (ArticleR "create"))) where
  parseJSON = wrappedParseJSON "ArticleCreate" "article"

newtype instance ArticleR "update" = ArticleUpdate (WithUpdate (ArticleR "all")) deriving (Generic)

instance FromJSON (ArticleR "update") where
  parseJSON =
    updatableParseJSON ["title", "description", "body"] $
      genericParseJSON @(WithUpdate (ArticleR "all")) defaultOptions

instance FromJSON (In (ArticleR "update")) where
  parseJSON = wrappedParseJSON "ArticleUpdate" "article"

-- FIXME
instance FromHttpApiData (ArticleR "id") where
  parseUrlPiece = undefined
