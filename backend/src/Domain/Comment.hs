{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Representations for comment
--
-- @since 0.1.0.0
module Comment (CommentR (..)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), defaultOptions, genericParseJSON)
import Data.Generic.HKD (construct)
import Data.UUID (UUID)
import Article (ArticleR)
import User (UserR)
import Util.Field (Time)
import Util.JSON.From (In, wrappedParseJSON)
import Util.JSON.To (Out, wrappedToEncoding, wrappedToJSON)
import Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import GHC.TypeLits (Symbol)
import Servant (FromHttpApiData)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | Type family for different representations of comments
--
-- @since 0.1.0.0
data family CommentR (r :: Symbol)

-- | Id which can be used to uniquely idenitify a comment.
--
-- @since 0.1.0.0
newtype instance CommentR "id" = CommentId UUID deriving newtype (Show, Eq, Hashable, ToJSON)

-- | @since 0.1.0.0
deriving via (WithNoValidation UUID) instance FromJSON (WithValidation (CommentR "id"))

-- | @since 0.1.0.0
deriving via (WithNoValidation UUID) instance FromHttpApiData (WithValidation (CommentR "id"))

-- | Representation in storage
--
-- @since 0.1.0.0
data instance CommentR "all" = Comment
  { id :: CommentR "id",
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:22:56.637Z",
    body :: Text, -- "It takes a Jacobian",
    author :: UserR "id",
    article :: ArticleR "id"
  }
  deriving (Generic)

--------------------------
--                 m    --
--  mmm   m   m  mm#mm  --
-- #" "#  #   #    #    --
-- #   #  #   #    #    --
-- "#m#"  "mm"#    "mm  --
--------------------------

-- | Representation for output
--
-- @since 0.1.0.0
data instance CommentR "withAuthorProfile" = CommentWithAuthorProfile
  { id :: CommentR "id",
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:22:56.637Z",
    body :: Text, -- "It takes a Jacobian",
    author :: UserR "profile"
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | @since 0.2.0.0
instance ToJSON (Out (CommentR "withAuthorProfile")) where
  toJSON = wrappedToJSON "comment"
  toEncoding = wrappedToEncoding "comment"

-- | @since 0.2.0.0
instance (Foldable t, ToJSON (t (CommentR "withAuthorProfile"))) => ToJSON (Out (t (CommentR "withAuthorProfile"))) where
  toJSON = wrappedToJSON "comments"
  toEncoding = wrappedToEncoding "comments"

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
newtype instance CommentR "create" = CommentCreate
  { body :: Text -- "It takes a Jacobian",
  }
  deriving (Eq, Show, Generic)

-- | @since 0.1.0.0
instance FromJSON (WithValidation (CommentR "create")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

-- | @since 0.1.0.0
instance FromJSON (In (WithValidation (CommentR "create"))) where
  parseJSON = wrappedParseJSON "CommentCreate" "comment"
-- ^
-- >>> eitherDecode' @(In (WithValidation (CommentR "create"))) "{ \"comment\": { \"body\": \"\"} }"
-- >>> eitherDecode' @(WithValidation (CommentR "create")) "{ \"body\": \"\"}"
-- Right (In (Success (CommentCreate {body = ""})))
-- Right (Success (CommentCreate {body = ""}))
