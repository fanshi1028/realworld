{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier for decoding JWT token
--
-- @since 0.4.0.0
module Effect.Token.Decode.JWT (DecodeTokenJWTC (runDecodeTokenJWT)) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Crypto.JWT (JWTError, decodeCompact, verifyClaimsAt)
import Data.Authentication.HasAuth (AuthOf)
import Data.Domain (Domain)
import Data.Field.Time (Time (Time))
import Data.Token.HasToken (TokenOf)
import Effect.Token.Decode (DecodeTokenE (DecodeToken), InvalidToken (InvalidToken))
import Relude.Extra (un)
import Servant.Auth.Server (FromJWT, JWTSettings, decodeJWT, validationKeys)
import Servant.Auth.Server.Internal.ConfigTypes (jwtSettingsToJwtValidationSettings)

-- | @since 0.3.0.0
newtype DecodeTokenJWTC (s :: Domain) (m :: Type -> Type) a = DecodeTokenJWTC
  { -- | @since 0.3.0.0
    runDecodeTokenJWT :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
-- Copy from servant auth but amended so that it is not in IO
verifyJWTAt :: (FromJWT a) => Time -> JWTSettings -> ByteString -> Maybe a
verifyJWTAt (Time time) jwtCfg input = runIdentity $ do
  verifiedJWT <- runExceptT $ do
    decodeCompact (fromStrict input)
      >>= verifyClaimsAt (jwtSettingsToJwtValidationSettings jwtCfg) (validationKeys jwtCfg) time
  pure $ case verifiedJWT of
    Left (_ :: JWTError) -> Nothing
    Right v -> case decodeJWT v of
      Left _ -> Nothing
      Right v' -> Just v'
{-# INLINE verifyJWTAt #-}

-- | @since 0.3.0.0
instance
  ( Member (R.Reader JWTSettings) sig,
    Member (Throw (InvalidToken s)) sig,
    Member (R.Reader Time) sig,
    Algebra sig m,
    FromJWT (AuthOf s),
    Coercible (TokenOf s) ByteString
  ) =>
  Algebra (DecodeTokenE s :+: sig) (DecodeTokenJWTC s m)
  where
  alg _ (L (DecodeToken token)) ctx = do
    verifyJWTAt <$> R.ask <*> R.ask ?? un @ByteString token
      >>= \case
        Nothing -> throwError $ InvalidToken token
        (Just auth) -> pure $ auth <$ ctx
  alg hdl (R other) ctx = DecodeTokenJWTC $ alg (runDecodeTokenJWT . hdl) other ctx
  {-# INLINE alg #-}