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
import Domain.User (UserR (UserAuthWithToken, UserToken))
import Domain.Util.Error (NotAuthorized)
import Domain.Util.Representation (Transform (transform))
import Network.Wai (Request, requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import qualified StmContainers.Map as STM (lookup)
import Storage.Map.InMem (TableInMem, TableInMem')
import Token (E (DecodeToken))
import Token.JWT (run)
import Token.JWT.Invalidate.Pure (run)

-- | @since 0.1.0.0
pattern HasToken :: UserR "token" -> Request
pattern HasToken token <- (List.lookup "authorization" . requestHeaders -> Just (parseHeader @(UserR "token") -> Right token))

-- | Make use of 'CookieSettings' and 'JWTSettings' from servant-auth
--
-- @since 0.1.0.0
data TokenAuth

-- | @since 0.1.0.0
instance IsAuth TokenAuth (UserR "authWithToken") where
  type AuthArgs TokenAuth = '[CookieSettings, JWTSettings]
  runAuth _ _ cs jwts = Auth.AuthCheck $
    \case
      HasToken token ->
        runM
          . runThrow @Error
          . runThrow @(NotAuthorized UserR)
          . R.runReader cs
          . R.runReader jwts
          . Token.JWT.Invalidate.Pure.run @UserR
          . Token.JWT.run @UserR
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
  type AuthArgs TokenAuthInMem = '[TableInMem UserR, TableInMem' UserR "token" "id"]
  runAuth _ _ userDb tokenDb = Auth.AuthCheck $ \case
    HasToken token ->
      atomically
        ( STM.lookup token tokenDb
            >>= traverse (`STM.lookup` userDb)
            <&> maybe mempty (flip UserAuthWithToken token <<$>> transform @_ @_ @"auth") . join
        )
    _ -> pure mempty
