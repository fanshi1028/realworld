{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Authentication.Pure (run, SomeNotLogin (..), SomeAlreadyLogin (..), SomeNotAuthorized (..)) where

import Authentication (E (Login, Logout))
import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Data.Singletons.Bool (SBoolI (sbool), fromSBool)
import GHC.TypeLits (Symbol)

-- FIXME: not yet used
data SomeNotAuthorized = SomeNotAuthorized deriving (Show, Generic)

data SomeAlreadyLogin = SomeAlreadyLogin deriving (Show, Generic)

data SomeNotLogin = SomeNotLogin deriving (Show, Generic)

newtype C (r :: Symbol -> Type) (b :: Bool) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( SBoolI b,
    Algebra sig m,
    Member (Throw SomeAlreadyLogin) sig,
    Member (Throw SomeNotLogin) sig
  ) =>
  Algebra (E r :+: sig) (C r b m)
  where
  alg _ (L (Login _)) ctx =
    if fromSBool $ sbool @b
      then throwError SomeAlreadyLogin
      else -- FIXME
        pure $ undefined <$ ctx
  alg _ (L Logout) ctx =
    if fromSBool $ sbool @b
      then pure $ () <$ ctx
      else throwError SomeNotLogin
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
