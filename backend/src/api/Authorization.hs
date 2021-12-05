{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.List as List (lookup)
import Data.Time.Clock (getCurrentTime)
import Domain (Domain (User))
import Domain.Transform (Transform (transform))
import Domain.User (UserR (UserAuthWithToken))
import Network.Wai (Request, requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import qualified StmContainers.Map as STM (lookup)
import qualified StmContainers.Map as STMMap (Map)
import InMem.Storage (TableInMem)
import InMem.Storage.Map (IdOf)
import Token (TokenOf (..))
import Token.Decode (E (DecodeToken), InvalidToken)
import Token.Decode.JWT (run)

-- | @since 0.1.0.0
-- extract token from request
pattern RequestToken :: TokenOf 'User -> Request
pattern RequestToken token <- (List.lookup "authorization" . requestHeaders -> Just (parseHeader @(TokenOf 'User) -> Right token))

-- | @since 0.1.0.0
-- make use of 'CookieSettings' and 'JWTSettings' from servant-auth
data TokenAuth

-- | @since 0.1.0.0
instance IsAuth TokenAuth (UserR "authWithToken") where
  type AuthArgs TokenAuth = '[CookieSettings, JWTSettings]
  runAuth _ _ _ jwts = Auth.AuthCheck $
    \case
      RequestToken token -> do
        time <- getCurrentTime
        DecodeToken token
          & send
          & Token.Decode.JWT.run @'User
          & R.runReader time
          & R.runReader jwts
          & runThrow @(InvalidToken 'User)
          & runM
          <&> \case
            Right auth -> pure $ UserAuthWithToken auth token
            _ -> mempty
      _ -> pure mempty

-- | @since 0.1.0.0
-- Use hand-roll in-memory storage to facilitate auth process
data TokenAuthInMem

-- | @since 0.1.0.0
instance IsAuth TokenAuthInMem (UserR "authWithToken") where
  type AuthArgs TokenAuthInMem = '[TableInMem 'User, STMMap.Map (TokenOf 'User) (IdOf 'User)]
  runAuth _ _ userDb tokenDb = Auth.AuthCheck $ \case
    RequestToken token ->
      atomically $
        STM.lookup token tokenDb
          >>= traverse (`STM.lookup` userDb)
          <&> \case
            (join -> Just u) -> pure $ UserAuthWithToken (transform u) token
            _ -> mempty
    _ -> pure mempty
