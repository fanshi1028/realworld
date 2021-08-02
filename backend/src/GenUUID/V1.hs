{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module GenUUID.V1 where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Data.UUID.V1 (nextUUID)
import GenUUID (E (Generate))

data RequestedUUIDsTooQuickly = RequestedUUIDsTooQuickly

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Member (Throw RequestedUUIDsTooQuickly) sig,
    Member (Lift IO) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L Generate) ctx = sendIO nextUUID >>= maybe (throwError RequestedUUIDsTooQuickly) (pure . (<$ ctx))
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
