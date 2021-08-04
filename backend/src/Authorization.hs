{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Authorization (TokenAuth, TokenAuthInMem) where

import Authentication.Pure (SomeNotAuthorized)
import Authentication.Token (E (CheckToken))
import Authentication.Token.JWT (run)
import Authentication.Token.JWT.Invalidate.Pure (run)
import Control.Algebra (send)
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R
import Control.Carrier.Throw.Either (runThrow)
import Crypto.JOSE (Error)
import qualified Data.List as List
import Domain.User (UserR (Token, UserAuthWithToken))
import Domain.Util.Representation (Transform (transform))
import Network.Wai (requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import qualified StmContainers.Map as STM
import Storage.InMem (TableInMem, TableInMem')

data TokenAuth

instance IsAuth TokenAuth (UserR "authWithToken") where
  type AuthArgs TokenAuth = '[CookieSettings, JWTSettings]
  runAuth _ _ cs jwts = Auth.AuthCheck $
    \case
      ( List.lookup "authorization" . requestHeaders ->
          Just (parseHeader @(UserR "token") -> Right token)
        ) ->
          runM
            . runThrow @Error
            . runThrow @SomeNotAuthorized
            . R.runReader cs
            . R.runReader jwts
            . Authentication.Token.JWT.Invalidate.Pure.run @UserR
            . Authentication.Token.JWT.run @UserR
            >=> \case
              Right (Right auth) -> pure $ pure $ UserAuthWithToken auth token
              _ -> pure mempty
            $ send (CheckToken token)
      _ -> pure mempty

data TokenAuthInMem

instance IsAuth TokenAuthInMem (UserR "auth") where
  type AuthArgs TokenAuthInMem = '[TableInMem UserR, TableInMem' UserR "token" "id"]
  runAuth _ _ userDb tokenDb = Auth.AuthCheck $ \case
    ( List.lookup "authorization" . requestHeaders ->
        Just (parseHeader @(UserR "token") -> Right token)
      ) ->
        atomically
          ( STM.lookup token tokenDb
              >>= traverse (`STM.lookup` userDb)
              <&> maybe mempty (transform @_ @_ @"auth") . join
          )
    _ -> pure mempty
