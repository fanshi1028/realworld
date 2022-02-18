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
-- @since 0.4.0.0
module API.Authorization where

import Control.Algebra (send)
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import Data.Domain (Domain (User))
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import Data.Field.Time (getCurrentTime)
import qualified Data.List as List (lookup)
import Data.Token.HasToken (TokenOf (..))
import Network.Wai (Request, requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import Token.Decode (DecodeTokenE (DecodeToken), InvalidToken)
import Token.Decode.JWT (DecodeTokenJWTC (runDecodeTokenJWT), runDecodeTokenJWT)

-- | @since 0.1.0.0
-- extract token from request
pattern RequestToken :: TokenOf 'User -> Request
pattern RequestToken token <- (List.lookup "authorization" . requestHeaders -> Just (parseHeader @(TokenOf 'User) -> Right token))

-- | @since 0.1.0.0
-- make use of 'CookieSettings' and 'JWTSettings' from servant-auth
data TokenAuth

-- | @since 0.4.0.0
instance IsAuth TokenAuth UserAuthWithToken where
  type AuthArgs TokenAuth = '[CookieSettings, JWTSettings]
  runAuth _ _ _ jwts = Auth.AuthCheck $
    \case
      RequestToken token -> do
        time <- getCurrentTime
        DecodeToken token
          & send
          & runDecodeTokenJWT @'User
          & R.runReader time
          & R.runReader jwts
          & runThrow @(InvalidToken 'User)
          & runM
          <&> \case
            Right auth -> pure $ UserAuthWithToken auth token
            _ -> mempty
      _ -> pure mempty
