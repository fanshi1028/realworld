{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Auth token for 'User'
--
-- @since 0.4.0.0
module Data.Token.Internal.HasToken.User where

import Data.Aeson (ToJSON (toJSON))
import Data.Domain (Domain (User))
import Servant (FromHttpApiData (parseUrlPiece))
import Data.Token.Internal.HasToken (HasToken (..))

-- | @since 0.3.0.0
instance HasToken 'User where
  newtype TokenOf 'User = UserToken ByteString deriving (Show, Eq, Hashable)

instance ToJSON (TokenOf 'User) where
  toJSON (UserToken bs) = toJSON $ decodeUtf8 @Text bs

-- | @since 0.2.0.0
instance FromHttpApiData (TokenOf 'User) where
  parseUrlPiece =
    parseUrlPiece @Text >=> \case
      (words -> [prefix, token])
        | (prefix == "Token") -> pure $ UserToken $ encodeUtf8 token
      _ -> Left "Authentication Header should be in format: \"Authorization: Token jwt.token.here\""
-- ^
-- Supposed to be used for Authorization header but not in url path
--
-- ==== __Fail__
-- >>> parseUrlPiece @(TokenOf 'User) "NotToken jwt.token.here"
-- Left "Authentication Header should be in format: \"Authorization: Token jwt.token.here\""
--
-- ==== __Success__
-- >>> parseUrlPiece @(TokenOf 'User) "Token jwt.token.here"
-- Right (UserToken "jwt.token.here")
