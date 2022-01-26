{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- All Server combined
--
-- @since 0.1.0.0
module Server where

import Effect.Authentication (AuthenticationE)
import Data.Authentication.HasAuth (AuthOf, NotLogin (NotLogin))
import qualified Data.Authentication.HasAuth as AuthErr (NotAuthorized (BadPassword, NoSuchUser))
import Control.Algebra (Algebra)
import qualified Control.Carrier.Reader as R (Reader, local)
import Control.Effect.Catch (Catch)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Effect.Cookie.Xsrf (CreateXsrfCookieE)
import Data.Domain (Domain (User))
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import API (Api)
import API.OptionalAuth (OptionalAuthApi)
import API.Protected (ProtectedApi)
import Effect.OptionalAuthAction (OptionalAuthActionE)
import Effect.OptionalAuthAction.Many (OptionalAuthActionManyE)
import Data.Paging (Limit, Offset)
import Servant (ServerT, type (:<|>) ((:<|>)))
import Servant.Auth.Server (AuthResult (Authenticated, BadPassword, Indefinite, NoSuchUser), CookieSettings, JWTSettings)
import Servant.Server (hoistServer)
import Server.Auth.User (authUserServer)
import Server.OptionalAuth (optionallyAuthedServer)
import Server.Protected (authedServer)
import Server.Public (publicServer)
import Data.Token.HasToken (TokenOf)
import Effect.Token.Create (CreateTokenE)
import Effect.Token.Decode (InvalidToken)
import Effect.UserAction (UserActionE)
import Effect.UserAction.Many (UserActionManyE)
import Data.Util.Validation (ValidationErr)
import Effect.VisitorAction (VisitorActionE)

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
            appendOptionalAuth = hoistServer (Proxy @OptionalAuthApi) $ case auth of
              Authenticated u -> appendAuth' u
              _ -> id
            appendAuth = hoistServer (Proxy @ProtectedApi) $ \eff -> case auth of
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