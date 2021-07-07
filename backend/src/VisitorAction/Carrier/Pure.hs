{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module VisitorAction.Carrier.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import VisitorAction.Effect (VisitorAction (GetAritcle, GetProfile, Register))

newtype VisitorActionPure m a = VisitorActionPure
  { runVisitorActionPure :: m a
  }
  deriving (Functor, Applicative, Monad)

-- FIXME
instance Algebra sig m => Algebra (VisitorAction :+: sig) (VisitorActionPure m) where
  alg hdl (L (Register ur)) ctx = pure $ undefined  <$ ctx
  alg hdl (L (GetProfile ur)) ctx = pure $ undefined <$ ctx
  alg hdl (L (GetAritcle ar)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = VisitorActionPure $ alg (runVisitorActionPure . hdl) other ctx
