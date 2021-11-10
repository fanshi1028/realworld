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
-- Carrier for invalidating JWT token (pure for now)
--
-- @since 0.3.0.0
module Token.Invalidate.JWT where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Domain (Domain)
import Token.Invalidate (E (InvalidateToken))

-- | @since 0.3.0.0
newtype C (s :: Domain) (m :: Type -> Type) a = C
  { -- | @since 0.3.0.0
    run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  (Algebra sig m) =>
  Algebra (E s :+: sig) (C s m)
  where
  alg _ (L (InvalidateToken _)) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
