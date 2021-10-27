{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- instance of user storage
--
-- @since 0.2.0.0
module Storage.Map.Internal.HasStorage.User where

import Data.Aeson (ToJSON)
import Field.Bio (Bio)
import Field.Email (Email)
import Field.Image (Image)
import Field.Password (PasswordHash)
import Field.Username (Username)
import GHC.Records (HasField, getField)
import Servant (FromHttpApiData)
import Storage.Map.Internal.HasStorage (HasStorage (..))
import Util.Validation (WithValidation)

-- | @since 0.2.0.0
instance HasStorage "user" where
  newtype IdOf "user" = UserId Username deriving (Show, Eq, Hashable, ToJSON)
  data ContentOf "user" = UserContent
    { email :: Email, -- "jake@jake.jake",
    -- token :: UserR "token", -- "jwt.token.here",
      password :: PasswordHash, -- "jakejake"
      username :: Username, -- "jake",
      bio :: Bio, -- "I work at statefarm",
      image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
    }
    deriving (Generic)

-- | @since 0.2.0.0
deriving via (WithValidation Username) instance FromHttpApiData (WithValidation (IdOf "user"))

-- | @since 0.2.0.0
toUserId :: HasField "username" u Username => u -> IdOf "user"
toUserId = UserId . getField @"username"
