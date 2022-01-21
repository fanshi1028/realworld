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
-- @since 0.4.0.0
module Effect.Token.Invalidate.JWT where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Data.Domain (Domain)
import Effect.Token.Invalidate (InvalidateTokenE (InvalidateToken))

-- | @since 0.3.0.0
newtype InvalidateTokenC (s :: Domain) (m :: Type -> Type) a = InvalidateTokenC
  { -- | @since 0.3.0.0
    runInvalidateToken :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.3.0.0
instance
  (Algebra sig m) =>
  Algebra (InvalidateTokenE s :+: sig) (InvalidateTokenC s m)
  where
  alg _ (L (InvalidateToken _)) ctx = pure $ () <$ ctx
  alg hdl (R other) ctx = InvalidateTokenC $ alg (runInvalidateToken . hdl) other ctx
  {-# INLINE alg #-}
