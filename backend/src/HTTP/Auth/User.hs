{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module HTTP.Auth.User (AuthUserApi, authUserServer) where

import Authentication (E (Login, Register))
import Control.Algebra (Algebra, send)
import Control.Effect.Catch (Catch)
import Control.Effect.Error (Throw, throwError)
import Control.Effect.Lift (Lift, sendIO)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Data.ByteString.Base64.Type (mkBS64)
import Domain.User (UserR (..))
import Domain.Util.Error (Impossible (Impossible), NotAuthorized, NotFound, ValidationErr)
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import Domain.Util.Validation (WithValidation)
import HTTP.Util (CreateApi)
import Relude.Extra (un)
import Servant
  ( HList (HCons),
    Header,
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
import Servant.Auth.Server (CookieSettings, JWTSettings, SetCookie, acceptLogin)
import qualified Storage.Map
import Token (E (CreateToken))
import Validation (validation)
import Web.Cookie (setCookieValue)

type AuthUserApi =
  ( "login" :> ReqBody '[JSON] (In (WithValidation (UserR "login")))
      :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Out (UserR "authWithToken")))
  )
    :<|> CreateApi UserR "authWithToken"

authUserServer ::
  ( Algebra sig m,
    Member (Throw ValidationErr) sig,
    Member (Lift IO) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader JWTSettings) sig,
    Member (Throw Impossible) sig,
    Member (Token.E UserR) sig,
    Member (Authentication.E UserR) sig,
    Member (Throw (NotAuthorized UserR)) sig,
    Member (Catch (NotFound (UserR "id"))) sig,
    Member (Storage.Map.E UserR) sig
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
                  Headers auth hs@(HCons h _) -> case h of
                    Header (UserToken . mkBS64 . setCookieValue -> jwt) ->
                      pure $ Headers (Out $ UserAuthWithToken auth jwt) hs
                    MissingHeader -> throwError $ Impossible "missing header for login"
                    UndecodableHeader bs -> throwError $ Impossible $ "undecodable header: " <> show bs
      )
        :<|> ( validateThen Register
                 >=> \auth -> Out . UserAuthWithToken auth <$> send (CreateToken auth)
             )
