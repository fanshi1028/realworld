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

import Authentication (NotLogin (NotLogin))
import qualified Authentication (E)
import qualified Authentication as AuthErr (NotAuthorized (BadPassword, NoSuchUser))
import Authorization (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader, local)
import Control.Effect.Catch (Catch)
import Control.Effect.Lift (Lift)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain (Domain (User))
import Domain.User (UserR)
import HTTP.Auth.User (AuthUserApi, authUserServer)
import HTTP.OptionalAuth (OptionallyAuthedApi, optionallyAuthedServer)
import HTTP.Protected (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import qualified OptionalAuthAction
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult (Authenticated, BadPassword, Indefinite, NoSuchUser), CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified Storage.Map (E)
import Token (InvalidToken)
import qualified Token (E)
import qualified UserAction (E)
import Util.Validation (ValidationErr)
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
    :> ( Auth '[TokenAuth] (UserR "authWithToken") :> (OptionallyAuthedApi :<|> AuthedApi)
           :<|> "users" :> AuthUserApi
           :<|> PublicApi
       )
    :<|> Get '[JSON] Text

-- * Server

-- | @since 0.1.0.0
-- all server
server ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member OptionalAuthAction.E sig,
    Member UserAction.E sig,
    Member (Lift IO) sig,
    Member (Token.E 'User) sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw Text) sig,
    Member (Catch (InvalidToken 'User)) sig,
    Member (Authentication.E 'User) sig,
    Member (Throw (InvalidToken 'User)) sig,
    Member (Throw (NotLogin 'User)) sig,
    Member (Storage.Map.E 'User) sig,
    Member (Throw (AuthErr.NotAuthorized 'User)) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig
  ) =>
  ServerT Api m
server =
  ( ( \auth ->
        let appendOptionalAuth = hoistServer (Proxy @OptionallyAuthedApi) $ case auth of
              Authenticated u -> R.local $ const $ Just u
              _ -> id
            appendAuth = hoistServer (Proxy @AuthedApi) $ \eff -> case auth of
              Authenticated u -> R.local (const $ Just u) eff
              BadPassword -> throwError $ AuthErr.BadPassword @'User
              NoSuchUser -> throwError $ AuthErr.NoSuchUser @'User
              Indefinite -> throwError $ NotLogin @'User
         in appendOptionalAuth optionallyAuthedServer :<|> appendAuth authedServer
    )
      :<|> authUserServer
      :<|> publicServer
  )
    :<|> pure "health-checked"
