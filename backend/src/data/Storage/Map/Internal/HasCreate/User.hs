{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Create 'User' in storage
--
-- @since 0.3.0.0
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

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.3.0.0
instance HasCreate 'User where
  data CreateOf 'User = UserCreate
    { username :: Username,
      email :: Email,
      password :: Password
    }
    deriving (Show, Generic)

-- | @since 0.3.0.0
instance FromJSON (WithValidation (CreateOf 'User)) where
  parseJSON = construct <<$>> genericParseJSON defaultOptions
-- ^
-- ==== Success
-- >>> eitherDecode' @(WithValidation (CreateOf 'User)) "{\"username\": \"\", \"email\": \"ff2239fj3902@fiew.mail\", \"password\":\"11fewifwofwwefew\" }"
-- Right (Success (UserCreate {username = "", email = "ff2239fj3902@fiew.mail", password = **PASSWORD**}))
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation (CreateOf 'User)) "{\"username\": \"\", \"email\": \"\", \"password\":\"11\" }"
-- Right (Failure ("null email" :| ["PasswordTooShort 8 2"]))
--
-- ==== Fail
-- >>> eitherDecode' @(WithValidation (CreateOf 'User)) "{\"username\": \"\", \"email\": \"ff2239fj3902@fiew.mail\"}"
-- Left "Error in $: parsing Storage.Map.Internal.HasCreate.User.CreateOf(UserCreate) failed, key \"password\" not found"

-- | @since 0.3.0.0
instance FromJSON (In (WithValidation (CreateOf 'User))) where
  parseJSON = wrappedParseJSON "UserRegister" "user"
-- ^
-- ==== Success
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'User))) "{ \"user\": {\"username\": \"\", \"email\": \"ff2239fj3902@fiew.mail\", \"password\":\"11fewifwofwwefew\" } }"
-- Right (In (Success (UserCreate {username = "", email = "ff2239fj3902@fiew.mail", password = **PASSWORD**})))
--
-- ==== Fail
-- >>> eitherDecode' @(In (WithValidation (CreateOf 'User))) "{\"username\": \"\", \"email\": \"ff2239fj3902@fiew.mail\", \"password\":\"11fewifwofwwefew\" }"
-- Left "Error in $: key \"user\" not found"
