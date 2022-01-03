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
import Authentication.HasAuth (AuthOf, NotLogin (NotLogin))
import qualified Authentication.HasAuth as AuthErr (NotAuthorized (BadPassword, NoSuchUser))
import Authorization (TokenAuth)
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader, local)
import Control.Effect.Catch (Catch)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Cookie.Xsrf (CreateXsrfCookieE)
import Domain (Domain (User))
import Domain.User (UserAuthWithToken (UserAuthWithToken))
import HTTP.Auth.User (AuthUserApi, authUserServer)
import HTTP.OptionalAuth (OptionallyAuthedApi, optionallyAuthedServer)
import HTTP.Protected (AuthedApi, authedServer)
import HTTP.Public (PublicApi, publicServer)
import OptionalAuthAction (OptionalAuthActionE)
import OptionalAuthAction.Many (OptionalAuthActionManyE)
import Paging (Limit, Offset)
import Servant (Get, JSON, ServerT, type (:<|>) ((:<|>)), type (:>))
import Servant.Auth.Server (Auth, AuthResult (Authenticated, BadPassword, Indefinite, NoSuchUser), CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import Token.Create (CreateTokenE)
import Token.Decode (InvalidToken)
import Token.HasToken (TokenOf)
import UserAction (UserActionE)
import UserAction.Many (UserActionManyE)
import Util.Validation (ValidationErr)
import VisitorAction (VisitorActionE)

-- * API

-- | @since 0.4.0.0
-- all api
--
-- __NOTE__: This part of api is for health check
--
-- > Get '[JSON] Text
type Api =
  "api"
    :> ( Auth '[TokenAuth] UserAuthWithToken :> (OptionallyAuthedApi :<|> AuthedApi)
           :<|> "users" :> AuthUserApi
           :<|> PublicApi
       )
    :<|> Get '[JSON] Text

-- * Server

-- | @since 0.1.0.0
-- all server
server ::
  ( Algebra sig m,
    Member (VisitorActionE []) sig,
    Member OptionalAuthActionE sig,
    Member (OptionalAuthActionManyE []) sig,
    Member UserActionE sig,
    Member (UserActionManyE []) sig,
    Member CreateXsrfCookieE sig,
    Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig,
    Member (AuthenticationE 'User) sig,
    Member (CreateTokenE 'User) sig,
    Member (Throw Text) sig,
    Member (Throw ValidationErr) sig,
    Member (Throw (NotLogin 'User)) sig,
    Member (Throw (AuthErr.NotAuthorized 'User)) sig,
    Member (Catch (InvalidToken 'User)) sig,
    Member (R.Reader (Maybe (TokenOf 'User))) sig,
    Member (R.Reader (Maybe (AuthOf 'User))) sig
  ) =>
  ServerT Api m
server =
  ( ( \auth ->
        let appendAuth' (UserAuthWithToken user token) = R.local (const $ Just user) . R.local (const $ Just token)
            appendOptionalAuth = hoistServer (Proxy @OptionallyAuthedApi) $ case auth of
              Authenticated u -> appendAuth' u
              _ -> id
            appendAuth = hoistServer (Proxy @AuthedApi) $ \eff -> case auth of
              Authenticated u -> appendAuth' u eff
              BadPassword -> throwError $ AuthErr.BadPassword @'User
              NoSuchUser -> throwError $ AuthErr.NoSuchUser @'User
              Indefinite -> throwError $ NotLogin @'User
         in appendOptionalAuth optionallyAuthedServer :<|> appendAuth authedServer
    )
      :<|> authUserServer
      :<|> publicServer
  )
    :<|> pure "health-checked"
