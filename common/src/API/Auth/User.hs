{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API for authorization
--
-- @since 0.4.0.0
module API.Auth.User where

import API.Util (CreateApi)
import Data.Authentication.HasAuth (HasAuth (LoginOf))
import Data.Domain (Domain (User))
import Data.Domain.User (UserAuthWithToken (..))
import Data.Util.JSON.From (In)
import Data.Util.JSON.To (Out)
import Data.Util.Validation (WithValidation)
import Servant
  ( Header,
    Headers,
    JSON,
    ReqBody,
    StdMethod (POST),
    Verb,
    type (:<|>),
    type (:>),
  )
import Web.Cookie (SetCookie)

-- * API

-- | Register or Login
--
-- @since 0.4.0.0
type AuthUserApi =
  ( "login" :> ReqBody '[JSON] (In (WithValidation (LoginOf 'User)))
      :> Verb 'POST 200 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Out UserAuthWithToken))
  )
    :<|> CreateApi 'User UserAuthWithToken
