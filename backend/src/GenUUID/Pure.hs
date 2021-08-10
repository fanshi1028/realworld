{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module GenUUID.Pure (run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Data.UUID (nil)
import GenUUID (E (Generate))

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (E :+: sig) (C m) where
  alg _ (L Generate) ctx = pure $ nil <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
