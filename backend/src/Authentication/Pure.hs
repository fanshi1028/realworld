{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Authentication.Pure (run) where

import Authentication (E (Login, Logout, Register))
import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.Util.Error (AlreadyLogin (AlreadyLogin), NotLogin (NotLogin))
import GHC.TypeLits (Symbol)

newtype C (r :: Symbol -> Type) (b :: Bool) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m, Member (Throw (AlreadyLogin r)) sig) => Algebra (E r :+: sig) (C r 'True m) where
  -- FIXME
  alg _ (L (Register a)) ctx = undefined
  alg _ (L (Login _)) _ = throwError $ AlreadyLogin @r
  alg _ (L Logout) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

instance (Algebra sig m, Member (Throw (NotLogin r)) sig) => Algebra (E r :+: sig) (C r 'False m) where
-- FIXME
  alg _ (L (Register a)) ctx = undefined
  alg _ (L (Login u)) ctx = pure $ undefined <$ ctx
  alg _ (L Logout) _ = throwError $ NotLogin @r
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
