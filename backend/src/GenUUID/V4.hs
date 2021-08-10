{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module GenUUID.V4 (run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import Data.UUID.V4 (nextRandom)
import GenUUID (E (Generate))

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance
  ( Member (Lift IO) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L Generate) ctx = (<$ ctx) <$> sendIO nextRandom
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
