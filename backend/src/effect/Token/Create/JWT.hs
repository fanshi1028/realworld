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
-- Carrier for creating JWT token
--
-- @since 0.4.0.0
module Token.Create.JWT (CreateTokenJWTC (runCreateTokenJWT)) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import qualified Control.Effect.Reader as R (Reader, ask, asks)
import qualified Control.Effect.State as S (State, get, put)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Crypto.JOSE (Error)
import Crypto.JWT (DRG, NumericDate (NumericDate), bestJWSAlg, claimExp, encodeCompact, newJWSHeader, signClaims, withDRG)
import Data.Authentication.HasAuth (AuthOf)
import Data.Domain (Domain)
import Data.Field.Time (Time (Time))
import Data.Authentication.HasToken (TokenOf)
import Relude.Extra (un, (.~))
import Servant.Auth.Server (CookieSettings (cookieExpires), JWTSettings, ToJWT, encodeJWT, jwtAlg, signingKey)
import Token.Create (CreateTokenE (CreateToken))

-- | @since 0.3.0.0
newtype CreateTokenJWTC (s :: Domain) gen (m :: Type -> Type) a = CreateTokenJWTC
  { -- | @since 0.3.0.0
    runCreateTokenJWT :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
-- Copy from servant auth but amended so that it is not in IO
makeJWT :: (ToJWT a, DRG gen) => a -> gen -> JWTSettings -> Maybe Time -> (Either Error LByteString, gen)
makeJWT v gen cfg expiry =
  withDRG gen $
    runExceptT $ do
      bestAlg <- bestJWSAlg $ signingKey cfg
      encodeCompact <$> signClaims (signingKey cfg) (newJWSHeader ((), fromMaybe bestAlg $ jwtAlg cfg)) (addExp $ encodeJWT v)
  where
    addExp = case expiry of
      Nothing -> id
      Just (Time e) -> claimExp .~ Just (NumericDate e)
{-# INLINE makeJWT #-}

-- | @since 0.3.0.0
instance
  ( Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (Throw Error) sig,
    Member (S.State gen) sig,
    Algebra sig m,
    DRG gen,
    ToJWT (AuthOf s),
    Coercible (TokenOf s) ByteString
  ) =>
  Algebra (CreateTokenE s :+: sig) (CreateTokenJWTC s gen m)
  where
  alg _ (L (CreateToken auth)) ctx = do
    (et, g) <- makeJWT auth <$> S.get @gen <*> R.ask <*> R.asks (fmap Time . cookieExpires)
    S.put g
    either throwError (pure . (<$ ctx) . un . toStrict) et
  alg hdl (R other) ctx = CreateTokenJWTC $ alg (runCreateTokenJWT . hdl) other ctx
  {-# INLINE alg #-}
