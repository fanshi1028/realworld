{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : API & Server
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- All API & Server combined
--
-- @since 0.1.0.0
module HTTP where

import qualified Authentication (E)
import Authorization (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader, local)
import Control.Effect.Catch (Catch)
import Control.Effect.Lift (Lift)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain.User (UserR)
import Domain.Util.Error (Impossible, NotAuthorized, ValidationErr)
import HTTP.Auth.User (AuthUserApi, authUserServer)
import HTTP.Protected (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult, CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified Storage.Map (E)
import qualified Token (E)
import qualified UserAction (E)
import qualified VisitorAction (E)

-- * API

-- | __NOTE__: This part of api
--
-- > Get '[JSON] Text
--
-- is for health check
--
-- @since 0.1.0.0
type Api =
  "api"
    :> ( Auth '[TokenAuth] (UserR "authWithToken") :> (PublicApi :<|> AuthedApi)
           :<|> "users" :> AuthUserApi
       )
    :<|> Get '[JSON] Text

-- * Server

-- | @since 0.1.0.0
server ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member UserAction.E sig,
    Member (Lift IO) sig,
    Member (Token.E UserR) sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader (AuthResult (UserR "authWithToken"))) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw Impossible) sig,
    Member (Catch (NotAuthorized UserR)) sig,
    Member (Authentication.E UserR) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Storage.Map.E UserR) sig
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
