{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import GHC.TypeLits (Symbol)
import Relation (E (IsRelated, Relate, Unrelate))

newtype C (r1 :: Symbol -> Type) (r2 :: Symbol -> Type) (b :: Bool) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance (Algebra sig m) => Algebra (E r1 r2 :+: sig) (C r1 r2 'True m) where
  alg _ (L (Relate _ _)) ctx = pure $ () <$ ctx
  alg _ (L (Unrelate _ _)) ctx = pure $ () <$ ctx
  alg _ (L (IsRelated _ _)) ctx = pure $ True <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

instance (Algebra sig m) => Algebra (E r1 r2 :+: sig) (C r1 r2 'False m) where
  alg _ (L (Relate _ _)) ctx = pure $ () <$ ctx
  alg _ (L (Unrelate _ _)) ctx = pure $ () <$ ctx
  alg _ (L (IsRelated _ _)) ctx = pure $ False <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
