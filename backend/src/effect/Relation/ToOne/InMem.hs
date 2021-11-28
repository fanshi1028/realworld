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
module Relation.ToOne.InMem (C (..), run) where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import qualified Focus (update)
import GHC.TypeLits (Symbol)
import Relation.ToOne (E (GetRelated, Relate, Unrelate))
import qualified StmContainers.Map as STM (Map, focus, insert, lookup)

-- | @since 0.3.0.0
newtype C (r1 :: Type) (r :: Symbol) (r2 :: Type) (m :: Type -> Type) a = C
  { -- | @since 0.1.0.0
    run' :: ReaderT (STM.Map r1 r2) m a
  }
  deriving (Functor, Applicative, Monad, MonadReader (STM.Map r1 r2))

-- | @since 0.3.0.0
instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    Eq r1,
    Eq r2,
    Hashable r1
  ) =>
  Algebra (E r1 r r2 :+: sig) (C r1 r r2 m)
  where
  alg _ (L action) ctx =
    ask
      >>= fmap (<$ ctx) . sendM @STM . case action of
        Relate r1 r2 -> STM.insert r2 r1
        Unrelate r1 r2 -> STM.focus (Focus.update (\v -> if v == r2 then Nothing else Just v)) r1
        GetRelated r1 -> STM.lookup r1
  alg hdl (R other) ctx = C $ alg (run' . hdl) (R other) ctx
  {-# INLINE alg #-}

-- | @since 0.1.0.0
run ::
  forall r1 r r2 a m.
  -- | the in-memory db
  STM.Map r1 r2 ->
  -- | the effect stack
  C r1 r r2 m a ->
  m a
run db = run' >>> usingReaderT db
