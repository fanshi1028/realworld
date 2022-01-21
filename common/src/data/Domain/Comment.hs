{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Representations for comment
--
-- @since 0.4.0.0
module Data.Domain.Comment where

import Data.Aeson (ToJSON (toEncoding, toJSON))
import Data.Domain (Domain (Comment))
import Data.Domain.User (UserProfile)
import Data.Field.Time (Time)
import Data.Storage.Map (IdOf)
import Data.Util.JSON.To (Out, wrappedToEncoding, wrappedToJSON)

-- $setup
-- >>> import Data.Aeson (encode)
-- >>> import Field.Username (Username (Username))
-- >>> import Field.Email (Email (Email))
-- >>> import Field.Bio (Bio (Bio))
-- >>> import Field.Image (Image (Image))
-- >>> import Storage.Map (IdOf (CommentId))
-- >>> import Authentication (AuthOf (UserAuth))
-- >>> import Domain.User (UserProfile)
-- >>> import Data.Time (UTCTime (UTCTime))
-- >>> import Data.UUID (nil)
-- >>> import Util.JSON.To (Out (Out))
-- >>> t = UTCTime (toEnum 0) (toEnum 0)
-- >>> exampleProfile = UserProfile (UserAuth (Email "jake@jake.jake") (Username "jake") (Bio "I work at statefarm") (Image "https://static.productionready.io/images/smiley-cyrus.jpg")) False
-- >>> example = CommentWithAuthorProfile (CommentId nil) t t "It takes a Jacobian" exampleProfile

-- | @since 0.4.0.0
-- >>> example
-- CommentWithAuthorProfile {id = CommentId 00000000-0000-0000-0000-000000000000, createdAt = 1858-11-17 00:00:00 UTC, updatedAt = 1858-11-17 00:00:00 UTC, body = "It takes a Jacobian", author = UserProfile {profile = UserAuth {email = "jake@jake.jake", username = "jake", bio = "I work at statefarm", image = "https://static.productionready.io/images/smiley-cyrus.jpg"}, following = False}}
data CommentWithAuthorProfile = CommentWithAuthorProfile
  { id :: IdOf 'Comment,
    createdAt :: Time,
    updatedAt :: Time,
    body :: Text,
    author :: UserProfile
  }
  deriving (Show, Eq, Generic)

-- | @since 0.4.0.0
instance ToJSON CommentWithAuthorProfile
-- ^
-- >>> encode example
-- "{\"author\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"},\"body\":\"It takes a Jacobian\",\"createdAt\":\"1858-11-17T00:00:00Z\",\"id\":\"00000000-0000-0000-0000-000000000000\",\"updatedAt\":\"1858-11-17T00:00:00Z\"}"

-- | @since 0.4.0.0
instance ToJSON (Out CommentWithAuthorProfile) where
  toJSON = wrappedToJSON "comment"
  toEncoding = wrappedToEncoding "comment"
-- ^
-- >>> encode $ Out example
-- "{\"comment\":{\"author\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"},\"body\":\"It takes a Jacobian\",\"createdAt\":\"1858-11-17T00:00:00Z\",\"id\":\"00000000-0000-0000-0000-000000000000\",\"updatedAt\":\"1858-11-17T00:00:00Z\"}}"

-- | @since 0.4.0.0
instance (Foldable t, ToJSON (t CommentWithAuthorProfile)) => ToJSON (Out (t CommentWithAuthorProfile)) where
  toJSON = wrappedToJSON "comments"
  toEncoding = wrappedToEncoding "comments"
-- ^
-- >>> encode $ Out [ example ]
-- "{\"comments\":[{\"author\":{\"email\":\"jake@jake.jake\",\"bio\":\"I work at statefarm\",\"following\":false,\"username\":\"jake\",\"image\":\"https://static.productionready.io/images/smiley-cyrus.jpg\"},\"body\":\"It takes a Jacobian\",\"createdAt\":\"1858-11-17T00:00:00Z\",\"id\":\"00000000-0000-0000-0000-000000000000\",\"updatedAt\":\"1858-11-17T00:00:00Z\"}]}"
