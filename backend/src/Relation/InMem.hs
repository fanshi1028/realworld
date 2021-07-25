{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.InMem where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import GHC.TypeLits (Symbol)
import Relation (E (IsRelated, Relate, Unrelate))
import qualified Relation.InMem.STM as STM

newtype C (r1 :: Symbol -> Type) (r2 :: Symbol -> Type) (m :: Type -> Type) a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance (Algebra sig m, MonadIO m, Member (STM.E r1 r2) sig) => Algebra (E r1 r2 :+: sig) (C r1 r2 m) where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      (Relate r1 r2) -> send STM.Relate >>= \f -> liftIO (atomically $ f r1 r2)
      (Unrelate r1 r2) -> send STM.Unrelate >>= \f -> liftIO (atomically $ f r1 r2)
      (IsRelated r1 r2) -> send STM.IsRelated >>= \f -> liftIO (atomically $ f r1 r2)
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
