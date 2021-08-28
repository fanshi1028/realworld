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
-- Carrier for pure invalidation (no invalidation)
--
-- @since 0.1.0.0
module Token.JWT.Invalidate.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import GHC.TypeLits (Symbol)
import Token.JWT.Invalidate (E (Invalidate))

-- | @since 0.1.0.0
newtype C (r :: Symbol -> Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance (Algebra sig m) => Algebra (E r :+: sig) (C r m) where
  alg _ (L (Invalidate _)) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
