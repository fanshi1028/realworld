{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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

import Authentication (E (Login, Register), HasAuth (LoginOf))
import Control.Algebra (Algebra, send)
import Control.Effect.Error (Throw, throwError)
import Control.Effect.Lift (Lift, sendIO)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Domain (Domain (User))
import HTTP.Util (CreateApi)
import Relude.Extra (un)
import Servant
  ( HList (HCons),
    Headers (Headers),
    JSON,
    ReqBody,
    ResponseHeader (Header, MissingHeader, UndecodableHeader),
    ServerT,
    StdMethod (POST),
    Verb,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Auth.Server (CookieSettings, JWTSettings, acceptLogin)
import qualified Storage.Map
import Token (E (CreateToken), TokenOf (UserToken))
import User (UserR (..))
import Util.Error (Impossible (Impossible), ValidationErr)
import Util.JSON.From (In (In))
import Util.JSON.To (Out (Out))
import Util.Validation (WithValidation)
import Validation (validation)
import Web.Cookie (setCookieValue)

-- * API

-- | Register or Login
--
-- @since 0.1.0.0
type AuthUserApi =
  ( "login" :> ReqBody '[JSON] (In (WithValidation (LoginOf 'User)))
      -- :> Verb 'POST 200 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Out (UserR "authWithToken")))
      :> Verb 'POST 200 '[JSON] (Out (UserR "authWithToken"))
  )
    :<|> CreateApi 'User (UserR "authWithToken")

-- * Server

-- | @since 0.1.0.0
authUserServer ::
  ( Algebra sig m,
    Member (Throw ValidationErr) sig,
    Member (Lift IO) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader JWTSettings) sig,
    Member (Throw Impossible) sig,
    Member (Token.E 'User) sig,
    Member (Authentication.E 'User) sig,
    -- Member (Throw (NotAuthorized UserR)) sig,
    Member (Storage.Map.E 'User) sig
  ) =>
  ServerT AuthUserApi m
authUserServer =
  let validateThen action = validation (throwError @ValidationErr) (send . action) . un
   in ( validateThen Login
          >=> \authInfo -> do
            acceptLogin <$> R.ask <*> R.ask <*> pure authInfo
              >>= sendIO
              >>= \case
                Nothing -> throwError $ Impossible "accept login failed"
                Just f -> case f authInfo of
                  -- Headers auth hs@(HCons h _) -> case h of
                  Headers auth (HCons h _) -> case h of
                    Header (UserToken . decodeUtf8 . setCookieValue -> jwt) ->
                      -- pure $ Headers (Out $ UserAuthWithToken auth jwt) hs
                      pure $ Out $ UserAuthWithToken auth jwt
                    MissingHeader -> throwError $ Impossible "missing header for login"
                    UndecodableHeader bs -> throwError $ Impossible $ "undecodable header: " <> show bs
      )
        :<|> ( validateThen Register
                 >=> \auth -> Out . UserAuthWithToken auth <$> send (CreateToken auth)
             )
