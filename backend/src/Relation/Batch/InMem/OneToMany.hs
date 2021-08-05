{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.Batch.InMem.OneToMany where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import Data.HashSet (insert)
import GHC.TypeLits (Symbol)
import qualified ListT
import Relation.Batch (E (GetRelated))
import qualified StmContainers.Multimap as STM

newtype
  C
    (r1 :: Type)
    (r :: Symbol)
    (r2 :: Type)
    (f :: Type -> Type)
    (m :: Type -> Type)
    a = C
  { run :: ReaderT (STM.Multimap r1 r2) m a
  }
  deriving (Functor, Applicative, Monad, MonadReader (STM.Multimap r1 r2))

instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    Eq r1,
    Eq r2,
    Hashable r1,
    Hashable r2
  ) =>
  Algebra (E r1 r r2 HashSet :+: sig) (C r1 r r2 HashSet m)
  where
  alg _ (L (GetRelated _ k)) ctx =
    ask
      >>= fmap (<$ ctx) . sendM @STM . ListT.fold (pure <<$>> flip insert) mempty . STM.listTByKey k
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
