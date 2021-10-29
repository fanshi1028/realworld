{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier to run in memory
--
-- @since 0.1.0.0
module Relation.ToMany.InMem where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import GHC.TypeLits (Symbol)
import qualified ListT (toList)
import Relation.ToMany (E (GetRelated, IsRelated, Relate, Unrelate, UnrelateByKey))
import qualified StmContainers.Multimap as STM (Multimap, delete, deleteByKey, insert, listTByKey, lookup)

-- | @since 0.1.0.0
newtype C (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a = C
  { -- | @since 0.1.0.0
    run' :: ReaderT (STM.Multimap r1 r2) m a
  }
  deriving (Functor, Applicative, Monad, MonadReader (STM.Multimap r1 r2))

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    Eq r1,
    Eq r2,
    Hashable r1,
    Hashable r2
  ) =>
  Algebra (E r1 r r2 :+: sig) (C r1 r r2 m)
  where
  alg _ (L action) ctx =
    ask
      >>= fmap (<$ ctx) . sendM @STM . case action of
        Relate k v -> STM.insert v k
        Unrelate k v -> STM.delete v k
        UnrelateByKey k -> STM.deleteByKey k
        IsRelated k v -> STM.lookup v k
        GetRelated k -> ListT.toList . STM.listTByKey k
  alg hdl (R other) ctx = C $ alg (run' . hdl) (R other) ctx
  {-# INLINE alg #-}

-- | @since 0.1.0.0
run ::
  forall r1 r r2 a m.
  -- | the in-memory db
  STM.Multimap r1 r2 ->
  -- | the effect stack
  C r1 r r2 m a ->
  m a
run db = run' >>> usingReaderT db
