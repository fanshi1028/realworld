{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

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

import Authentication (NotLogin)
import qualified Authentication (E)
import Authorization (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader, local)
import Control.Effect.Catch (Catch)
import Control.Effect.Lift (Lift)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain (Domain (User))
import Domain.User (UserR)
import HTTP.Auth.User (AuthUserApi, authUserServer)
import HTTP.Protected (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult, CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified Storage.Map (E)
import Token (InvalidToken)
import qualified Token (E)
import qualified UserAction (E)
import Util.Error (Impossible, ValidationErr)
import qualified VisitorAction (E)

-- * API

-- | @since 0.1.0.0
-- all api
--
-- __NOTE__: This part of api is for health check
--
-- > Get '[JSON] Text
type Api =
  "api"
    :> ( Auth '[TokenAuth] (UserR "authWithToken") :> (PublicApi :<|> AuthedApi)
           :<|> "users" :> AuthUserApi
       )
    :<|> Get '[JSON] Text

-- * Server

-- | @since 0.1.0.0
-- all server
server ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member UserAction.E sig,
    Member (Lift IO) sig,
    Member (Token.E 'User) sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader (AuthResult (UserR "authWithToken"))) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw Impossible) sig,
    Member (Catch (InvalidToken 'User)) sig,
    Member (Authentication.E 'User) sig,
    Member (Throw (InvalidToken 'User)) sig,
    Member (Throw (NotLogin 'User)) sig,
    Member (Storage.Map.E 'User) sig
  ) =>
  ServerT Api m
server =
  ( ( \auth ->
        hoistServer
          (Proxy @(PublicApi :<|> AuthedApi))
          (R.local $ const auth)
          (publicServer :<|> authedServer)
    )
      :<|> authUserServer
  )
    :<|> pure "health-checked"
