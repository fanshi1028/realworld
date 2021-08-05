{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.Batch.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import GHC.TypeLits (Symbol)
import Relation.Batch (E (GetRelated))

newtype
  C
    (r1 :: Type)
    (r :: Symbol)
    (r2 :: Type)
    (f :: Type -> Type)
    (m :: Type -> Type)
    a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m, Monoid (f r2)) => Algebra (E r1 r r2 f :+: sig) (C r1 r r2 f m) where
  alg _ (L (GetRelated _ _)) ctx = pure $ mempty <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
