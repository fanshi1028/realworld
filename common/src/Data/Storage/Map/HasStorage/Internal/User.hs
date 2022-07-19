{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Storage for 'User'
--
-- @since 0.4.0.0
module Data.Storage.Map.HasStorage.Internal.User where

import Data.Aeson (ToJSON)
import Data.Domain (Domain (User))
import Data.Field.Bio (Bio)
import Data.Field.Email (Email)
import Data.Field.Image (Image)
import Data.Field.Password (PasswordHash)
import Data.Field.Username (Username)
import Data.Storage.Map.HasStorage.Internal (HasStorage (..))
import Data.Util.Validation (WithValidation)
import GHC.Records (HasField, getField)
import Servant (FromHttpApiData)

-- | @since 0.3.0.0
instance HasStorage 'User where
  newtype IdOf 'User = UserId Username deriving (Show, Eq, Hashable, ToJSON)
  data ContentOf 'User = UserContent
    { email :: Email, -- "jake@jake.jake",
    -- token :: TokenOf 'User, -- "jwt.token.here",
      password :: PasswordHash, -- "jakejake"
      username :: Username, -- "jake",
      bio :: Bio, -- "I work at statefarm",
      image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
    }
    deriving (Generic)

-- | @since 0.3.0.0
deriving via (WithValidation Username) instance FromHttpApiData (WithValidation (IdOf 'User))

-- * Helper

-- | @since 0.3.0.0
toUserId :: HasField "username" u Username => u -> IdOf 'User
toUserId = UserId . getField @"username"
