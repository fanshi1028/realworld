{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Auth for 'User'
--
-- @since 0.4.0.0
module Data.Authentication.HasAuth.Internal.User where

import Data.Aeson (FromJSON (parseJSON), ToJSON, defaultOptions, genericParseJSON)
import Data.Authentication.HasAuth.Internal (HasAuth (..))
import Data.Domain (Domain (User))
import Data.Field.Bio (Bio)
import Data.Field.Email (Email)
import Data.Field.Image (Image)
import Data.Field.Password (Password)
import Data.Field.Username (Username)
import Data.Generic.HKD (construct)
import Data.Util.JSON.From (In, wrappedParseJSON)
import Data.Util.Validation (WithValidation)
import Servant.Auth.Server (FromJWT, ToJWT)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.2.0.0
instance HasAuth 'User where
  data LoginOf 'User = UserLogin
    { email :: Email,
      password :: Password
    }
    deriving (Show, Generic)
  data AuthOf 'User = UserAuth
    { email :: Email, -- "jake@jake.jake",
    -- token :: TokenOf 'User, -- "jwt.token.here",
      username :: Username, -- "jake",
      bio :: Bio, -- "I work at statefarm",
      image :: Image -- "https://static.productionready.io/images/smiley-cyrus.jpg",
    }
    deriving (Show, Eq, Generic)

-- | @since 0.2.0.0
instance FromJSON (LoginOf 'User)

-- | @since 0.2.0.0
instance FromJSON (WithValidation (LoginOf 'User)) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions
-- ^
-- ==== Success
-- >>> eitherDecode' @(WithValidation (LoginOf 'User)) "{\"email\": \"ejfowfow@\", \"password\":\"11832hf92hf2389f\" }"
-- Right (Success (UserLogin {email = "ejfowfow@", password = **PASSWORD**}))
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation (LoginOf 'User)) "{\"email\": \"\", \"password\":\"112\" }"
-- Right (Failure ("null email" :| ["PasswordTooShort 8 3"]))
--
-- ==== Fail
-- >>> eitherDecode' @(WithValidation (LoginOf 'User)) "{\"email\": \"ejfowfow@\"}"
-- Left "Error in $: parsing Authentication.Internal.HasAuth.User.LoginOf(UserLogin) failed, key \"password\" not found"

-- | @since 0.2.0.0
instance FromJSON (In (WithValidation (LoginOf 'User))) where
  parseJSON = wrappedParseJSON "UserLogin" "user"
-- ^
-- ==== Success
-- >>> eitherDecode' @(In (WithValidation (LoginOf 'User))) "{ \"user\": {\"email\": \"ejfowfow@\", \"password\":\"11239h2389f9328\" } }"
-- Right (In (Success (UserLogin {email = "ejfowfow@", password = **PASSWORD**})))
--
-- ==== Validation Fail
-- >>> eitherDecode' @(In (WithValidation (LoginOf 'User))) "{ \"user\": {\"email\": \"\", \"password\":\"112\" } }"
-- Right (In (Failure ("null email" :| ["PasswordTooShort 8 3"])))
--
-- ==== Fail
-- >>> eitherDecode' @(In (WithValidation (LoginOf 'User))) "{\"email\": \"ejfowfow@\", \"password\":\"11239h2389f9328\" }"
-- Left "Error in $: key \"user\" not found"
--
-- >>> eitherDecode' @(In (WithValidation (LoginOf 'User))) "{ \"user\": {\"email\": \"2hf923h239@\"} }"
-- Left "Error in $.user: parsing Authentication.Internal.HasAuth.User.LoginOf(UserLogin) failed, key \"password\" not found"

-- | @since 0.2.0.0
instance FromJSON (AuthOf 'User)

-- | @since 0.2.0.0
instance ToJSON (AuthOf 'User)

-- | @since 0.2.0.0
instance FromJWT (AuthOf 'User)

-- | @since 0.2.0.0
instance ToJWT (AuthOf 'User)
