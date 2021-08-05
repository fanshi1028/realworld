{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module HTTP.Public.User (UserApi, userServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Error (Throw, throwError)
import Control.Effect.Lift (Lift, sendIO)
import qualified Control.Effect.Reader as R
import Control.Effect.Sum (Member)
import Data.ByteString.Base64.Type (mkBS64)
import Domain.User (UserR (..))
import Domain.Util.Error (ValidationErr)
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (CreateApi)
import Relude.Extra (un)
import Servant
  ( GetHeaders (getHeaders),
    HList (HCons),
    Header,
    Headers (Headers, getHeadersHList),
    JSON,
    ReqBody,
    ResponseHeader (Header, MissingHeader, UndecodableHeader),
    ServerT,
    StdMethod (POST),
    Verb,
    lookupResponseHeader,
    type (:<|>) ((:<|>)),
    type (:>),
  )
import Servant.Auth.Server (CookieSettings, JWTSettings, SetCookie, acceptLogin, makeJWT)
import Validation (validation)
import Domain.Util.Validation (WithValidation)
import VisitorAction (E (Login, Register))
import Web.Cookie (setCookieValue)

type UserApi =
  "login" :> ReqBody '[JSON] (In (WithValidation (UserR "login")))
    :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Out (UserR "authWithToken")))
      :<|> CreateApi UserR "authWithToken"

userServer ::
  ( Algebra sig m,
    Member (Throw ValidationErr) sig,
    Member VisitorAction.E sig,
    Member (Lift IO) sig,
    Member (R.Reader CookieSettings) sig,
    Member (R.Reader JWTSettings) sig
  ) =>
  ServerT UserApi m
userServer =
  let validateThen action = validation (throwError @ValidationErr) (send . action) . un
   in ( validateThen Login >=> \authInfo -> do
          acceptLogin <$> R.ask <*> R.ask <*> pure authInfo
            >>= sendIO
            >>= \case
              Nothing -> undefined
              Just f -> case f authInfo of
                Headers auth hs@(HCons h _) -> case h of
                  Header (Token . mkBS64 . setCookieValue -> jwt) ->
                    pure $ Headers (Out $ UserAuthWithToken auth jwt) hs
                  -- FIXME
                  MissingHeader -> undefined
                  -- FIXME
                  UndecodableHeader _ -> undefined
      )
        :<|> ( validateThen Register >=> \auth -> do
                 jwts <- R.ask
                 -- FIXME what should the expiry be?
                 sendIO (makeJWT auth jwts Nothing) >>= \case
                   Right (Token . mkBS64 . toStrict -> token) -> pure $ Out $ UserAuthWithToken auth token
                   -- FIXME
                   _ -> undefined
             )
