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
-- Carrier in pure
--
-- @since 0.1.0.0
module GenUUID.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Data.UUID (nil)
import GenUUID (E (Generate))

-- | @since 0.1.0.0
newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.1.0.0
instance (Algebra sig m) => Algebra (E :+: sig) (C m) where
  alg _ (L Generate) ctx = pure $ nil <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
  {-# INLINE alg #-}
