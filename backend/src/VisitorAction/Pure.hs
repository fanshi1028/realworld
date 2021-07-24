{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module VisitorAction.Pure (run) where

import qualified Authentication as Auth (E (Login))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Domain.User (UserR (..))
import Domain.Util.Error (NotAuthorized (NotAuthorized))
import Domain.Util.Representation (Transform (transform))
import qualified Storage (E (GetById))
import VisitorAction (E (GetAritcle, GetProfile, Login, Register))

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

-- FIXME
instance
  ( Member (Throw (NotAuthorized UserR)) sig,
    Member (Auth.E UserR) sig,
    Member (Storage.E UserR) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg hdl (L (Register user)) ctx = pure $ undefined <$ ctx
  alg hdl (L (Login user)) ctx =
    send (Auth.Login user)
      >>= send . Storage.GetById @UserR
      >>= \case
        Nothing -> throwError $ NotAuthorized @UserR
        Just user' -> pure $ transform user' <$ ctx
  alg hdl (L (GetProfile ur)) ctx = pure $ undefined <$ ctx
  alg hdl (L (GetAritcle ar)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
