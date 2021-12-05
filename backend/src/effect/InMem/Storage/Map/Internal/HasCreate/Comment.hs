{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Create 'Comment' in storage
--
-- @since 0.2.0.0
module InMem.Storage.Map.Internal.HasCreate.Comment where

import Data.Aeson (FromJSON (parseJSON), defaultOptions, genericParseJSON)
import Data.Generic.HKD (construct)
import Domain (Domain (Comment))
import InMem.Storage.Map.Internal.HasCreate (HasCreate (CreateOf))
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
-- ==== Success
-- >>> eitherDecode' @(WithValidation (CreateOf 'Comment)) "{ \"body\": \"\"}"
-- Right (Success (CommentCreate {body = ""}))
--
-- ==== Fail
-- >>> eitherDecode' @(WithValidation (CreateOf 'Comment)) "{}"
-- Left "Error in $: parsing InMem.Storage.Map.Internal.HasCreate.Comment.CreateOf(CommentCreate) failed, key \"body\" not found"

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (CreateOf 'Comment))) where
  parseJSON = wrappedParseJSON "CommentCreate" "comment"
-- ^
-- ==== Success
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'Comment))) "{ \"comment\": { \"body\": \"\"} }"
-- Right (In (Success (CommentCreate {body = ""})))
--
-- ==== Fail
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'Comment))) "{ \"body\": \"\"}"
-- Left "Error in $: key \"comment\" not found"
