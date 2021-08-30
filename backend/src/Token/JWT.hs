{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier for JWT
--
-- @since 0.1.0.0
module Token.JWT where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import qualified Control.Effect.Reader as R (Reader, ask, asks)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Crypto.JOSE (Error)
import Domain.Util.Error (NotAuthorized (NotAuthorized))
import GHC.TypeLits (Symbol)
import Relude.Extra (un)
import Servant.Auth.Server (CookieSettings (cookieExpires), FromJWT, JWTSettings, ToJWT, makeJWT, verifyJWT)
import Token (E (DecodeToken, CreateToken, InvalidateToken))
import Token.JWT.Invalidate (E (Invalidate))

-- | @since 0.1.0.0
newtype C (r :: Symbol -> Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( Member (R.Reader JWTSettings) sig,
    Member (R.Reader CookieSettings) sig,
    Member (Throw (NotAuthorized r)) sig,
    Member (Token.JWT.Invalidate.E r) sig,
    Member (Throw Error) sig,
    Member (Lift IO) sig,
    Algebra sig m,
    FromJWT (r "auth"),
    ToJWT (r "auth"),
    Coercible (r "token") Text
  ) =>
  Algebra (Token.E r :+: sig) (C r m)
  where
  alg _ (L (DecodeToken token)) ctx = do
    verifyJWT <$> R.ask <*> pure (encodeUtf8 $ un @Text token)
      >>= sendIO
      >>= \case
        Nothing -> throwError $ NotAuthorized @r
        (Just auth) -> pure $ auth <$ ctx
  alg _ (L (CreateToken auth)) ctx =
    makeJWT auth <$> R.ask <*> R.asks cookieExpires >>= sendIO
      >>= either throwError (pure . (<$ ctx) . un . decodeUtf8 @Text)
  alg _ (L (InvalidateToken token)) ctx = (<$ ctx) <$> send (Invalidate token)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
