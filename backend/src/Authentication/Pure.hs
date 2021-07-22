{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Authentication.Pure (run, SomeNotLogin, SomeAlreadyLogin) where

import Authentication (E (Login, Logout))
import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import GHC.TypeLits (Symbol)

-- FIXME: not yet used
data SomeNotAuthorized = SomeNotAuthorized deriving (Show, Generic)

data SomeAlreadyLogin = SomeAlreadyLogin deriving (Show, Generic)

data SomeNotLogin = SomeNotLogin deriving (Show, Generic)

newtype C (r :: Symbol -> Type) (b :: Bool) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Algebra sig m, Member (Throw SomeAlreadyLogin) sig) => Algebra (E r :+: sig) (C r 'True m) where
  alg _ (L (Login _)) _ = throwError SomeAlreadyLogin
  alg _ (L Logout) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m, Member (Throw SomeNotLogin) sig) => Algebra (E r :+: sig) (C r 'False m) where
  alg _ (L (Login u)) ctx = pure $ undefined <$ ctx
  alg _ (L Logout) _ = throwError SomeNotLogin
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
