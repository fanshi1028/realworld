{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @since 0.2.0.0
module Storage.Map.Internal.HasStorage.Comment where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Field.Time (Time)
import Servant (FromHttpApiData)
import Storage.Map.Internal.HasStorage (HasStorage (..))
import Util.Validation (NoValidation (..), WithNoValidation, WithValidation)

-- | @since 0.2.0.0
instance HasStorage "comment" where
  newtype IdOf "comment" = CommentId UUID deriving (Show, Eq, Hashable, ToJSON)
  data ContentOf "comment" = Comment
    { id :: IdOf "comment",
      createdAt :: Time, -- "2016-02-18T03:22:56.637Z",
      updatedAt :: Time, -- "2016-02-18T03:22:56.637Z",
      body :: Text, -- "It takes a Jacobian",
      author :: IdOf "user",
      article :: IdOf "article"
    }
    deriving (Generic)

-- | @since 0.2.0.0
deriving via (WithNoValidation UUID) instance FromJSON (WithValidation (IdOf "comment"))

-- | @since 0.2.0.0
deriving via (WithNoValidation UUID) instance FromHttpApiData (WithValidation (IdOf "comment"))
