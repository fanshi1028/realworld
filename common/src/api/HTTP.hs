{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- All API & Server combined
--
-- @since 0.1.0.0
module HTTP where

import Authorization (TokenAuth)
import Domain.User (UserAuthWithToken)
import HTTP.Auth.User (AuthUserApi)
import HTTP.OptionalAuth (OptionalAuthApi)
import HTTP.Protected (ProtectedApi)
import HTTP.Public (PublicApi)
import Servant (Get, JSON, type (:<|>), type (:>))
import Servant.Auth.Server (Auth)

-- * API

-- | @since 0.4.0.0
-- all api
--
-- __NOTE__: This part of api is for health check
--
-- > Get '[JSON] Text
type Api =
  "api"
    :> ( Auth '[TokenAuth] UserAuthWithToken :> (OptionalAuthApi :<|> ProtectedApi)
           :<|> "users" :> AuthUserApi
           :<|> PublicApi
       )
    :<|> Get '[JSON] Text
