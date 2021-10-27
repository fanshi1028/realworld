{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier for JWT
--
-- @since 0.1.0.0
module Token.JWT where

import Authentication (AuthOf)
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import qualified Control.Effect.Reader as R (Reader, ask, asks)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Crypto.JOSE (Error)
import Domain (Domain)
import Relude.Extra (un)
import Servant.Auth.Server (CookieSettings (cookieExpires), FromJWT, JWTSettings, ToJWT, makeJWT, verifyJWT)
import Token (E (CreateToken, DecodeToken, InvalidateToken), TokenOf)
import Token.JWT.Invalidate (E (Invalidate))
import Util.Error (NotAuthorized (NotAuthorized))

-- | @since 0.1.0.0
newtype C (s :: Domain) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (Throw (NotAuthorized (TokenOf s))) sig,
    Member (Token.JWT.Invalidate.E (TokenOf s)) sig,
    Member (Throw Error) sig,
    Member (Lift IO) sig,
    Algebra sig m,
    FromJWT (AuthOf s),
    ToJWT (AuthOf s),
    Coercible (TokenOf s) Text
  ) =>
  Algebra (Token.E s :+: sig) (C s m)
  where
  alg _ (L (DecodeToken token)) ctx = do
    verifyJWT <$> R.ask <*> pure (encodeUtf8 $ un @Text token)
      >>= sendIO
      >>= \case
        Nothing -> throwError $ NotAuthorized token
        (Just auth) -> pure $ auth <$ ctx
  alg _ (L (CreateToken auth)) ctx =
    makeJWT auth <$> R.ask <*> R.asks cookieExpires
      >>= sendIO
      >>= either throwError (pure . (<$ ctx) . un . decodeUtf8 @Text)
  alg _ (L (InvalidateToken token)) ctx = (<$ ctx) <$> send (Invalidate token)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
