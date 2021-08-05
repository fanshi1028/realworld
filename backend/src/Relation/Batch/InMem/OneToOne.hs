{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.Batch.InMem.OneToOne where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import GHC.TypeLits (Symbol)
import Relation.Batch (E (GetRelated))
import qualified StmContainers.Map as STM

newtype
  C
    (r1 :: Type)
    (r :: Symbol)
    (r2 :: Type)
    (f :: Type -> Type)
    (m :: Type -> Type)
    a = C
  { run :: ReaderT (STM.Map r1 r2) m a
  }
  deriving (Functor, Applicative, Monad, MonadReader (STM.Map r1 r2))

instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    Eq r1,
    Eq r2,
    Hashable r1
  ) =>
  Algebra (E r1 r r2 Maybe :+: sig) (C r1 r r2 Maybe m)
  where
  alg _ (L (GetRelated _ r1)) ctx = ask >>= fmap (<$ ctx) . sendM @STM . STM.lookup r1
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
