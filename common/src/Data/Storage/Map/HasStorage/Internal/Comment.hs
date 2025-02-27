{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Storage for 'Comment'
--
-- @since 0.4.0.0
module Data.Storage.Map.HasStorage.Internal.Comment where

import Data.Aeson (FromJSON, ToJSON)
import Data.Domain (Domain (Article, Comment, User))
import Data.Field.Time (Time)
import Data.Storage.Map.HasStorage.Internal (HasStorage (..))
import Data.UUID (UUID)
import Data.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import Servant (FromHttpApiData)

-- | @since 0.3.0.0
instance HasStorage 'Comment where
  newtype IdOf 'Comment = CommentId UUID deriving (Show, Eq, Hashable, ToJSON)
  data ContentOf 'Comment = CommentContent
    { id :: IdOf 'Comment,
      createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
      updatedAt :: Time, -- "2016-02-18T03:22:56.637Z",
      body :: Text, -- "It takes a Jacobian",
      author :: IdOf 'User,
      article :: IdOf 'Article
    }
    deriving (Generic)

-- | @since 0.3.0.0
deriving via (WithNoValidation UUID) instance FromJSON (WithValidation (IdOf 'Comment))

-- | @since 0.3.0.0
deriving via (WithNoValidation UUID) instance FromHttpApiData (WithValidation (IdOf 'Comment))
