{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier for pure invalidation (no invalidation)
--
-- @since 0.1.0.0
module Token.JWT.Invalidate.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Token.JWT.Invalidate (E (Invalidate))

-- | @since 0.1.0.0
newtype C token (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance (Algebra sig m) => Algebra (E token :+: sig) (C token m) where
  alg _ (L (Invalidate _)) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
