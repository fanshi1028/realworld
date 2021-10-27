{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Token.Internal.HasToken.User where

import Data.Aeson (ToJSON)
import Domain (Domain (User))
import Servant (FromHttpApiData (parseUrlPiece))
import Token.Internal.HasToken (HasToken (..))

instance HasToken 'User where
  newtype TokenOf 'User = UserToken Text deriving (Show, Eq, Hashable, ToJSON)

instance FromHttpApiData (TokenOf 'User) where
  parseUrlPiece =
    parseUrlPiece @Text >=> \case
      (words -> [prefix, token])
        | (prefix == "Token") -> pure $ UserToken token
      _ -> Left "Authentication Header should be in format: \"Authorization: Token jwt.token.here\""
