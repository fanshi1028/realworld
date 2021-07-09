{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module VisitorAction.Pure (run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import VisitorAction (E (GetAritcle, GetProfile, Register))

newtype C m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

-- FIXME
instance Algebra sig m => Algebra (E :+: sig) (C m) where
  alg hdl (L (Register ur)) ctx = pure $ undefined <$ ctx
  alg hdl (L (GetProfile ur)) ctx = pure $ undefined <$ ctx
  alg hdl (L (GetAritcle ar)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
