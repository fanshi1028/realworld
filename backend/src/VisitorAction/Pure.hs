{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module VisitorAction.Pure (run) where

import qualified Authorization as Auth (E (Login))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.User (UserR)
import Domain.Util.Error (NotAuthorized (NotAuthorized))
import VisitorAction (E (GetAritcle, GetProfile, Login, Register))

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- FIXME
instance
  ( Member (Throw (NotAuthorized UserR)) sig,
    Member (Auth.E UserR) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg hdl (L (Register user)) ctx = pure $ undefined <$ ctx
  alg hdl (L (Login user)) ctx =
    send (Auth.Login user)
      >>= maybe (throwError $ NotAuthorized @UserR) (pure . (<$ ctx))
  alg hdl (L (GetProfile ur)) ctx = pure $ undefined <$ ctx
  alg hdl (L (GetAritcle ar)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
