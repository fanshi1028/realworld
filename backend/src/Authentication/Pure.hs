{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Authentication.Pure (runTrue, runFalse) where

import Authentication (E (GetAuthInfo, Login, Logout, Register))
import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import GHC.TypeLits (Symbol)

newtype CTrue (r :: Symbol -> Type) m a = CTrue
  { runTrue :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Algebra sig m => Algebra (E r :+: sig) (CTrue r m) where
  -- FIXME
  alg _ (L GetAuthInfo) ctx = pure $ Just undefined <$ ctx
  alg _ (L (Register _)) ctx = pure $ undefined <$ ctx
  alg _ (L (Login _)) ctx = pure $ undefined <$ ctx
  alg _ (L Logout) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = CTrue $ alg (runTrue . hdl) other ctx

newtype CFalse (r :: Symbol -> Type) m a = CFalse
  { runFalse :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Algebra sig m => Algebra (E r :+: sig) (CFalse r m) where
  -- FIXME
  alg _ (L GetAuthInfo) ctx = pure $ Nothing <$ ctx
  alg _ (L (Register _)) ctx = pure $ undefined <$ ctx
  alg _ (L (Login _)) ctx = pure $ undefined <$ ctx
  alg _ (L Logout) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = CFalse $ alg (runFalse . hdl) other ctx
