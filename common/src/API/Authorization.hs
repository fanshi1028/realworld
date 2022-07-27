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

import Data.Domain (Domain (User))
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import qualified Data.List as List (lookup)
import Data.Authentication.HasToken (TokenOf (..))
import Network.Wai (Request, requestHeaders)
import Servant (FromHttpApiData (parseHeader))
import Servant.Auth.Server (CookieSettings, JWTSettings, verifyJWT)
import qualified Servant.Auth.Server as Auth (AuthCheck (AuthCheck))
import Servant.Auth.Server.Internal.Class (IsAuth (AuthArgs, runAuth))

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
  runAuth _ _ _ jwts = Auth.AuthCheck $ \case
    RequestToken token@(UserToken bs) ->
      verifyJWT jwts bs <&> \case
        Nothing -> mempty
        Just auth -> pure $ UserAuthWithToken auth token
    _ -> pure mempty
