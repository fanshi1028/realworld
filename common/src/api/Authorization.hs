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
-- @since 0.3.0.0
module Authorization where

import Control.Algebra (send)
import Control.Carrier.Lift (runM)
import qualified Control.Carrier.Reader as R (runReader)
import Control.Carrier.Throw.Either (runThrow)
import qualified Data.List as List (lookup)
import Domain (Domain (User))
import Domain.User (UserAuthWithToken (UserAuthWithToken))
import Field.Time (getCurrentTime)
import Network.Wai (Request, requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))
import Token.Decode (DecodeTokenE (DecodeToken), InvalidToken)
import Token.Decode.JWT (DecodeTokenJWTC (runDecodeTokenJWT), runDecodeTokenJWT)
import Token.HasToken (TokenOf (..))

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
