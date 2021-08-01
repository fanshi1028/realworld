{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Domain.Comment where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), defaultOptions, genericParseJSON)
import Data.Generic.HKD (construct)
import Data.UUID (UUID)
import Domain.User (UserR)
import Domain.Util.Field (Time)
import Domain.Util.JSON.From (In, wrappedParseJSON)
import Domain.Util.JSON.To (Out, wrappedToEncoding)
import GHC.TypeLits (Symbol)
import Servant (FromHttpApiData (parseUrlPiece))
import Validation.Carrier.Selective (WithValidation)
import Domain.Article (ArticleR)

data family CommentR (r :: Symbol)

newtype instance CommentR "id" = CommentId UUID deriving newtype (Show, Eq, Hashable, ToJSON)

data instance CommentR "all" = Comment
  { id :: CommentR "id",
    createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
    updatedAt :: Time, -- "2016-02-18T03:22:56.637Z",
    body :: Text, -- "It takes a Jacobian",
    author :: UserR "id",
    article :: ArticleR "id"
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
-- ^ >>> import Data.Aeson
--  >>> eitherDecode @(In (WithValidation (CommentR "create"))) "{ \"comment\": { \"body\": \"\"} }"
--  >>> eitherDecode @(WithValidation (CommentR "create")) "{ \"body\": \"\"}"

-- FIXME
instance FromHttpApiData (CommentR "id") where
  parseUrlPiece = undefined
