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

import Authentication (AuthenticationE)
import Authentication.HasAuth (NotLogin (NotLogin))
import qualified Authentication.HasAuth as AuthErr (NotAuthorized (BadPassword, NoSuchUser))
import Authorization (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader, local)
import Control.Effect.Catch (Catch)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Cookie.Xsrf (E)
import Domain (Domain (User))
import Domain.User (UserR)
import HTTP.Auth.User (AuthUserApi, authUserServer)
import HTTP.OptionalAuth (OptionallyAuthedApi, optionallyAuthedServer)
import HTTP.Protected (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import OptionalAuthAction (OptionalAuthActionE)
import Paging (Limit, Offset)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult (Authenticated, BadPassword, Indefinite, NoSuchUser), CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import qualified Token.Create (E)
import Token.Decode (InvalidToken)
import UserAction (UserActionE)
import Util.Validation (ValidationErr)
import VisitorAction (VisitorActionE)

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
    Member VisitorActionE sig,
    Member OptionalAuthActionE sig,
    Member UserActionE sig,
    Member Cookie.Xsrf.E sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig,
    Member (R.Reader (Maybe (UserR "authWithToken"))) sig,
    Member (AuthenticationE 'User) sig,
    Member (Token.Create.E 'User) sig,
    Member (Throw Text) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotLogin 'User)) sig,
    Member (Throw (AuthErr.NotAuthorized 'User)) sig,
    Member (Catch (InvalidToken 'User)) sig
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
