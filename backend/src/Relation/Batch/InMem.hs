{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.Batch.InMem where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import GHC.TypeLits (Symbol)
import Relation.Batch (E (GetRelated))
import qualified Relation.Batch.InMem.STM as STM

newtype C (r1 :: Symbol -> Type) (r2 :: Symbol -> Type) (f :: Type -> Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance
  ( Algebra sig m,
    Member (STM.E r1 r2 HashSet) sig,
    MonadIO m
  ) =>
  Algebra (E r1 r2 HashSet :+: sig) (C r1 r2 HashSet m)
  where
  alg _ (L (GetRelated r1)) ctx = send STM.GetRelated >>= liftIO . atomically . ($ r1) <&> (<$ ctx)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
