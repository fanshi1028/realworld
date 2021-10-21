{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Some IsAuth instances for servant
--
-- @since 0.1.0.0
module Authorization
  ( -- * TokenAuth
    TokenAuth,

    -- * TokenAuthInMem
    TokenAuthInMem,
  )
where

import Control.Algebra (send)
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Crypto.JOSE (Error)
import qualified Data.List as List (lookup)
import Network.Wai (Request, requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import qualified StmContainers.Map as STM (lookup)
import Storage.Map (IdOf)
import Storage.Map.InMem (TableInMem, TableInMem')
import Token (E (DecodeToken), TokenOf (..))
import Token.JWT (run)
import Token.JWT.Invalidate.Pure (run)
import User (UserR (UserAuthWithToken))
import Util.Error (NotAuthorized)
import Util.Representation (Transform (transform))

-- | @since 0.1.0.0
pattern RequestToken :: TokenOf "user" -> Request
pattern RequestToken token <- (List.lookup "authorization" . requestHeaders -> Just (parseHeader @(TokenOf "user") -> Right token))

-- | Make use of 'CookieSettings' and 'JWTSettings' from servant-auth
--
-- @since 0.1.0.0
data TokenAuth

-- | @since 0.1.0.0
instance IsAuth TokenAuth (UserR "authWithToken") where
  type AuthArgs TokenAuth = '[CookieSettings, JWTSettings]
  runAuth _ _ cs jwts = Auth.AuthCheck $
    \case
      RequestToken token ->
        runM
          . runThrow @Error
          . runThrow @(NotAuthorized (TokenOf "user"))
          . R.runReader cs
          . R.runReader jwts
          . Token.JWT.Invalidate.Pure.run @(TokenOf "user")
          . Token.JWT.run @"user"
          >=> \case
            Right (Right auth) -> pure $ pure $ UserAuthWithToken auth token
            _ -> pure mempty
          $ send $ DecodeToken token
      _ -> pure mempty

-- | Use hand-roll in-memory storage to facilitate auth process
--
-- @since 0.1.0.0
data TokenAuthInMem

-- | @since 0.1.0.0
instance IsAuth TokenAuthInMem (UserR "authWithToken") where
  type AuthArgs TokenAuthInMem = '[TableInMem "user", TableInMem' (TokenOf "user") (IdOf "user")]
  runAuth _ _ userDb tokenDb = Auth.AuthCheck $ \case
    RequestToken token ->
      atomically $
        STM.lookup token tokenDb
          >>= traverse (`STM.lookup` userDb)
          <&> \case
            (join -> Just u) -> pure $ UserAuthWithToken (transform u) token
            _ -> mempty
    _ -> pure mempty
