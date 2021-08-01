{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import GHC.TypeLits (Symbol)
import Relation (E (GetRelated, IsRelated, Relate, Unrelate))

newtype
  C
    (r1 :: Symbol -> Type)
    (idx1 :: Symbol)
    (r2 :: Symbol -> Type)
    (idx2 :: Symbol)
    (f :: Type -> Type)
    (b :: Bool)
    (m :: Type -> Type)
    a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m, Monoid (f (r2 idx2))) => Algebra (E r1 idx1 r2 idx2 f :+: sig) (C r1 idx1 r2 idx2 f 'False m) where
  alg _ (L (Relate _ _)) ctx = pure $ () <$ ctx
  alg _ (L (Unrelate _ _)) ctx = pure $ () <$ ctx
  alg _ (L (IsRelated _ _)) ctx = pure $ False <$ ctx
  -- alg _ (L (GetRelated _)) ctx = pure $ mempty <$ ctx
  alg _ (L (GetRelated _)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

instance (Algebra sig m, Monoid (f (r2 idx2))) => Algebra (E r1 idx1 r2 idx2 f :+: sig) (C r1 idx1 r2 idx2 f 'True m) where
  alg _ (L (Relate _ _)) ctx = pure $ () <$ ctx
  alg _ (L (Unrelate _ _)) ctx = pure $ () <$ ctx
  alg _ (L (IsRelated _ _)) ctx = pure $ True <$ ctx
  -- alg _ (L (GetRelated _)) ctx = pure $ mempty <$ ctx
  alg _ (L (GetRelated _)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
