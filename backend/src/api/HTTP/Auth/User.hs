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

import Authentication (AuthOf, E (Login, Register), HasAuth (LoginOf))
import Control.Algebra (Algebra, send)
import Control.Effect.Error (Throw, throwError)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Cookie.Xsrf (E (CreateXsrfCookie))
import Domain (Domain (User))
import Domain.User (UserR (..))
import HTTP.Util (CreateApi)
import Relude.Extra (un)
import Servant
  ( AddHeader,
    Header,
    Headers,
    JSON,
    ReqBody,
    ServerT,
    StdMethod (POST),
    Verb,
    addHeader,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Auth.Server (CookieSettings, JWTSettings)
import Servant.Auth.Server.Internal.Cookie (applyCookieSettings, applySessionCookieSettings)
import qualified Storage.Map (E)
import Token (TokenOf (UserToken))
import Token.Create (E (CreateToken))
import Util.JSON.From (In (In))
import Util.JSON.To (Out (Out))
import Util.Validation (ValidationErr, WithValidation)
import Validation (validation)
import Web.Cookie (SetCookie, def, setCookieValue)

-- * API

-- | Register or Login
--
-- @since 0.1.0.0
type AuthUserApi =
  ( "login" :> ReqBody '[JSON] (In (WithValidation (LoginOf 'User)))
      :> Verb 'POST 200 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Out (UserR "authWithToken")))
  )
    :<|> CreateApi 'User (UserR "authWithToken")

-- * Server

-- | @since 0.1.0.0
authUserServer ::
  ( Algebra sig m,
    Member (Throw ValidationErr) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader JWTSettings) sig,
    Member (Throw Text) sig,
    Member (Token.Create.E 'User) sig,
    Member (Authentication.E 'User) sig,
    Member (Storage.Map.E 'User) sig,
    Member Cookie.Xsrf.E sig,
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
