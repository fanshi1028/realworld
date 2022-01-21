{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Server for authorization
--
-- @since 0.1.0.0
module Server.Auth.User where

import Effect.Authentication (AuthenticationE (Login, Register))
import Data.Authentication.HasAuth (HasAuth (AuthOf))
import Control.Algebra (Algebra, send)
import Control.Effect.Error (Throw, throwError)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Effect.Cookie.Xsrf (CreateXsrfCookieE (CreateXsrfCookie))
import Data.Domain (Domain (User))
import Data.Domain.User (UserAuthWithToken (..))
import API.Auth.User (AuthUserApi)
import Relude.Extra (un)
import Servant
  ( AddHeader,
    ServerT,
    addHeader,
    type (:<|>) ((:<|>)),
  )
import Servant.Auth.Server (CookieSettings, JWTSettings)
import Servant.Auth.Server.Internal.Cookie (applyCookieSettings, applySessionCookieSettings)
import Effect.Token.Create (CreateTokenE (CreateToken))
import Data.Token.HasToken (TokenOf (UserToken))
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out (Out))
import Data.Util.Validation (ValidationErr)
import Validation (validation)
import Web.Cookie (SetCookie, def, setCookieValue)

-- * Server

-- | @since 0.1.0.0
authUserServer ::
  ( Algebra sig m,
    Member (Throw ValidationErr) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader JWTSettings) sig,
    Member (Throw Text) sig,
    Member (CreateTokenE 'User) sig,
    Member (AuthenticationE 'User) sig,
    Member CreateXsrfCookieE sig,
    AddHeader "Set-Cookie" SetCookie (AuthOf 'User) withOneCookie,
    AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies
  ) =>
  ServerT AuthUserApi m
authUserServer =
  let validateThen action = validation (throwError @ValidationErr) (send . action) . un
   in ( validateThen Login
          >=> \authInfo -> do
            token@(UserToken jwt) <- send $ CreateToken authInfo
            cookieSettings <- R.ask
            xsrfCookie <- send CreateXsrfCookie
            let sessionCookie =
                  applySessionCookieSettings cookieSettings $
                    applyCookieSettings cookieSettings $ def {setCookieValue = jwt}
            pure $
              addHeader sessionCookie $
                addHeader xsrfCookie $
                  Out $ UserAuthWithToken authInfo token
      )
        :<|> ( validateThen Register
                 >=> \auth -> Out . UserAuthWithToken auth <$> send (CreateToken auth)
             )
