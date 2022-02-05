{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- All API
--
-- @since 0.4.0.0
module API where

import API.Auth.User (AuthUserApi)
import API.Authorization (TokenAuth)
import API.OptionalAuth (OptionalAuthApi)
import API.Protected (ProtectedApi)
import API.Public (PublicApi)
import Data.Domain.User (UserAuthWithToken)
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
