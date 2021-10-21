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
-- @since 0.1.0.0
module Comment where

import Data.Aeson (ToJSON (toEncoding, toJSON))
import Field.Time (Time)
import GHC.TypeLits (Symbol)
import Storage.Map (IdOf)
import User (UserR)
import Util.JSON.To (Out, wrappedToEncoding, wrappedToJSON)

-- | Type family for different representations of comments
--
-- @since 0.1.0.0
data family CommentR (r :: Symbol)

-- | Representation for output
--
-- @since 0.2.0.0
data instance CommentR "withAuthorProfile" = CommentWithAuthorProfile
  { id :: IdOf "comment",
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:22:56.637Z",
    body :: Text, -- "It takes a Jacobian",
    author :: UserR "profile"
  }
  deriving (Show, Eq, Generic)

instance ToJSON (CommentR "withAuthorProfile")

-- | @since 0.2.0.0
instance ToJSON (Out (CommentR "withAuthorProfile")) where
  toJSON = wrappedToJSON "comment"
  toEncoding = wrappedToEncoding "comment"

-- | @since 0.2.0.0
instance (Foldable t, ToJSON (t (CommentR "withAuthorProfile"))) => ToJSON (Out (t (CommentR "withAuthorProfile"))) where
  toJSON = wrappedToJSON "comments"
  toEncoding = wrappedToEncoding "comments"
