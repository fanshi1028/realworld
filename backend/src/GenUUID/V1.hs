{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier for V1 UUID generation
--
-- @since 0.1.0.0
module GenUUID.V1 where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Data.UUID.V1 (nextUUID)
import GenUUID (E (Generate))

-- | Error for V1 generation
-- when requested UUIDs too quickly
--
-- @since 0.1.0.0
data RequestedUUIDsTooQuickly = RequestedUUIDsTooQuickly deriving (Show)

-- | @since 0.1.0.0
newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( Member (Throw RequestedUUIDsTooQuickly) sig,
    Member (Lift IO) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L Generate) ctx = sendIO nextUUID >>= maybe (throwError RequestedUUIDsTooQuickly) (pure . (<$ ctx))
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
