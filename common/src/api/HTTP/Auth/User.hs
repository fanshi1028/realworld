{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server for authorization
--
-- @since 0.1.0.0
module HTTP.Auth.User where

import Authentication.HasAuth (HasAuth (LoginOf))
import Domain (Domain (User))
import Domain.User (UserAuthWithToken (..))
import HTTP.Util (CreateApi)
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
import Util.JSON.From (In)
import Util.JSON.To (Out)
import Util.Validation (WithValidation)
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
