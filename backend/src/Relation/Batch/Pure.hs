{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.Batch.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import GHC.TypeLits (Symbol)
import Relation.Batch (E (GetRelated))

newtype C (r1 :: Symbol -> Type) (r2 :: Symbol -> Type) (f :: Type -> Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance (Algebra sig m, Monoid (f (r2 "id"))) => Algebra (E r1 r2 f :+: sig) (C r1 r2 f m) where
  alg _ (L (GetRelated _)) ctx = pure $ mempty <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
