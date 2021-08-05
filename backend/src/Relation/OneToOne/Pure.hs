{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.OneToOne.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import GHC.TypeLits (Symbol)
import Relation.OneToOne (E (GetRelated, IsRelated, Relate, Unrelate))

newtype
  C
    (r1 :: Type)
    (r :: Symbol)
    (r2 :: Type)
    (b :: Bool)
    (m :: Type -> Type)
    a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad)

instance (Algebra sig m) => Algebra (E r1 r r2 :+: sig) (C r1 r r2 'False m) where
  alg _ (L Relate {}) ctx = pure $ () <$ ctx
  alg _ (L Unrelate {}) ctx = pure $ () <$ ctx
  alg _ (L IsRelated {}) ctx = pure $ False <$ ctx
  alg _ (L GetRelated {}) ctx = pure $ Nothing <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

instance (Algebra sig m) => Algebra (E r1 r r2 :+: sig) (C r1 r r2 'True m) where
  alg _ (L Relate {}) ctx = pure $ () <$ ctx
  alg _ (L Unrelate {}) ctx = pure $ () <$ ctx
  alg _ (L IsRelated {}) ctx = pure $ True <$ ctx
  -- FIXME
  alg _ (L GetRelated {}) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
