{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @since 0.2.0.0
module Storage.Map.Internal.HasCreate.User where

import Data.Aeson (FromJSON (parseJSON), defaultOptions, genericParseJSON)
import Data.Generic.HKD (construct)
import Domain (Domain (User))
import Field.Email (Email)
import Field.Password (Password)
import Field.Username (Username)
import Storage.Map.Internal.HasCreate (HasCreate (CreateOf))
import Util.JSON.From (In, wrappedParseJSON)
import Util.Validation (WithValidation)

-- | @since 0.2.0.0
instance HasCreate 'User where
  data CreateOf 'User = UserCreate
    { username :: Username,
      email :: Email,
      password :: Password
    }
    deriving (Show, Generic)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (CreateOf 'User)) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions
-- ^
-- >>> import Data.Aeson (eitherDecode')
-- >>> eitherDecode' @(WithValidation (CreateOf 'User)) "{\"username\": \"\", \"email\": \"ff2239fj3902@fiew.mail\", \"password\":\"11fewifwofwwefew\" }"
-- Right (Success (UserCreate {username = "", email = "ff2239fj3902@fiew.mail", password = **PASSWORD**}))

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (CreateOf 'User))) where
  parseJSON = wrappedParseJSON "UserRegister" "user"
-- ^
-- >>> import Data.Aeson (eitherDecode')
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'User))) "{ \"user\": {\"username\": \"\", \"email\": \"\", \"password\":\"11\" } }"
-- Right (In (Failure ("null email" :| ["PasswordTooShort 8 2"])))
