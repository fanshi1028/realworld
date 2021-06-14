{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Domain.Comment where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), defaultOptions, genericParseJSON)
import Data.Generic.HKD (construct)
import Data.UUID (UUID)
import Domain.User (UserR)
import Domain.Util (Out, Time, wrappedToEncoding, In, wrappedParseJSON)
import GHC.TypeLits (Symbol)
import Validation.Adaptor (WithValidation)

data family CommentR (r :: Symbol)

newtype instance CommentR "id" = CommentId UUID deriving newtype (ToJSON)

data instance CommentR "all" = Comment
  { id :: CommentR "id",
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:22:56.637Z",
    body :: Text, -- "It takes a Jacobian",
    author :: UserR "id"
  }

--------------------------
--                 m    --
--  mmm   m   m  mm#mm  --
-- #" "#  #   #    #    --
-- #   #  #   #    #    --
-- "#m#"  "mm"#    "mm  --
--------------------------

data instance CommentR "withAuthorProfile" = CommentWithAuthorProfile
  { id :: CommentR "id",
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:22:56.637Z",
    body :: Text, -- "It takes a Jacobian",
    author :: UserR "profile"
  }
  deriving (Generic, ToJSON)

instance ToJSON (Out (CommentR "withAuthorProfile")) where
  toEncoding = wrappedToEncoding "comment"

instance (Foldable t, ToJSON (t (CommentR "withAuthorProfile"))) => ToJSON (Out (t (CommentR "withAuthorProfile"))) where
  toEncoding = wrappedToEncoding "comments"

-------------------
--   "           --
-- mmm    m mm   --
--   #    #"  #  --
--   #    #   #  --
-- mm#mm  #   #  --
-------------------

newtype instance CommentR "create" = CommentCreate
  { body :: Text -- "It takes a Jacobian",
  }
  deriving (Generic, Show)

instance FromJSON (WithValidation (CommentR "create")) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions

instance FromJSON (In (WithValidation (CommentR "create"))) where
  parseJSON = wrappedParseJSON "CommentCreate" "comment"
-- ^>>> import Data.Aeson
-- >>> eitherDecode @(In (WithValidation (CommentR "create"))) "{ \"comment\": { \"body\": \"\"} }"
-- >>> eitherDecode @(WithValidation (CommentR "create")) "{ \"body\": \"\"}"
