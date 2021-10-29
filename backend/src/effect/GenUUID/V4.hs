{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier for V4 'UUID' generation
--
-- @since 0.1.0.0
module GenUUID.V4 where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendIO)
import Control.Effect.Sum (Member)
import Data.UUID.V4 (nextRandom)
import GenUUID (E (Generate))

-- | @since 0.1.0.0
newtype C m a = C
  { -- | @since 0.1.0.0
    run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance
  ( Member (Lift IO) sig,
    Algebra sig m
  ) =>
  Algebra (E :+: sig) (C m)
  where
  alg _ (L Generate) ctx = (<$ ctx) <$> sendIO nextRandom
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}