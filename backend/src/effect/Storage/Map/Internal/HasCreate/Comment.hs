{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | @since 0.2.0.0
module Storage.Map.Internal.HasCreate.Comment where

import Data.Aeson (FromJSON (parseJSON), defaultOptions, genericParseJSON)
import Data.Generic.HKD (construct)
import Domain (Domain (Comment))
import Storage.Map.Internal.HasCreate (HasCreate (CreateOf))
import Util.JSON.From (In, wrappedParseJSON)
import Util.Validation (WithValidation)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.2.0.0
instance HasCreate 'Comment where
  newtype CreateOf 'Comment = CommentCreate
    { body :: Text -- "It takes a Jacobian",
    }
    deriving (Show, Generic)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (CreateOf 'Comment)) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions
-- ^
-- >>> eitherDecode' @(WithValidation (CreateOf 'Comment)) "{ \"body\": \"\"}"
-- Right (Success (CommentCreate {body = ""}))

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (CreateOf 'Comment))) where
  parseJSON = wrappedParseJSON "CommentCreate" "comment"
-- ^
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'Comment))) "{ \"comment\": { \"body\": \"\"} }"
-- Right (In (Success (CommentCreate {body = ""})))
