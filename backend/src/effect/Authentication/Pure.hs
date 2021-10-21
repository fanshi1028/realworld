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

import Authentication (E (Login, Logout, Register), HasAuth (LoginOf))
import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import GHC.TypeLits (Symbol)
import Util.Error (AlreadyLogin (AlreadyLogin), NotLogin (NotLogin))

-- | @since 0.1.0.0
newtype C (s :: Symbol) (b :: Bool) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance (Algebra sig m, Member (Throw (AlreadyLogin (LoginOf s))) sig) => Algebra (E s :+: sig) (C s 'True m) where
  -- FIXME
  alg _ (L (Register a)) ctx = undefined
  alg _ (L (Login l)) _ = throwError $ AlreadyLogin l
  alg _ (L Logout) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}

-- | @since 0.1.0.0
instance (Algebra sig m, Member (Throw (NotLogin ())) sig) => Algebra (E s :+: sig) (C s 'False m) where
  -- FIXME
  alg _ (L (Register a)) ctx = undefined
  alg _ (L (Login u)) ctx = pure $ undefined <$ ctx
  alg _ (L Logout) _ = throwError $ NotLogin ()
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
