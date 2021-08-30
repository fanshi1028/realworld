{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- Carrier in pure
--
-- @since 0.1.0.0
module Authentication.Pure where

import Authentication (E (Login, Logout, Register))
import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.Util.Error (AlreadyLogin (AlreadyLogin), NotLogin (NotLogin))
import GHC.TypeLits (Symbol)

-- | @since 0.1.0.0
newtype C (r :: Symbol -> Type) (b :: Bool) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance (Algebra sig m, Member (Throw (AlreadyLogin r)) sig) => Algebra (E r :+: sig) (C r 'True m) where
  -- FIXME
  alg _ (L (Register a)) ctx = undefined
  alg _ (L (Login _)) _ = throwError $ AlreadyLogin @r
  alg _ (L Logout) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}

-- | @since 0.1.0.0
instance (Algebra sig m, Member (Throw (NotLogin r)) sig) => Algebra (E r :+: sig) (C r 'False m) where
  -- FIXME
  alg _ (L (Register a)) ctx = undefined
  alg _ (L (Login u)) ctx = pure $ undefined <$ ctx
  alg _ (L Logout) _ = throwError $ NotLogin @r
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
