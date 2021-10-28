{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Auth token for 'User'
--
-- @since 0.2.0.0
module Token.Internal.HasToken.User where

import Data.Aeson (ToJSON)
import Domain (Domain (User))
import Servant (FromHttpApiData (parseUrlPiece))
import Token.Internal.HasToken (HasToken (..))

-- | @since 0.2.0.0
instance HasToken 'User where
  newtype TokenOf 'User = UserToken Text deriving (Show, Eq, Hashable, ToJSON)

-- | @since 0.2.0.0
instance FromHttpApiData (TokenOf 'User) where
  parseUrlPiece =
    parseUrlPiece @Text >=> \case
      (words -> [prefix, token])
        | (prefix == "Token") -> pure $ UserToken token
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
