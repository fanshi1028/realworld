{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.InMem.OneToMany where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import Data.HashSet (insert)
import GHC.TypeLits (Symbol)
import qualified ListT
import Relation (E (GetRelated, IsRelated, Relate, Unrelate))
import qualified StmContainers.Multimap as STM

newtype
  C
    (r1 :: Symbol -> Type)
    (idx1 :: Symbol)
    (r2 :: Symbol -> Type)
    (idx2 :: Symbol)
    (f :: Type -> Type)
    (m :: Type -> Type)
    a = C
  { run :: ReaderT (STM.Multimap (r1 idx1) (r2 idx2)) m a
  }
  deriving (Functor, Applicative, Monad, MonadReader (STM.Multimap (r1 idx1) (r2 idx2)))

instance
  ( Algebra sig m,
    Member (Lift STM) sig,
    Eq (r1 idx1),
    Eq (r2 idx2),
    Hashable (r1 idx1),
    Hashable (r2 idx2)
  ) =>
  Algebra (E r1 idx1 r2 idx2 HashSet :+: sig) (C r1 idx1 r2 idx2 HashSet m)
  where
  alg _ (L action) ctx =
    ask
      >>= fmap (<$ ctx) . sendM @STM . case action of
        Relate k v -> STM.insert v k
        Unrelate k v -> STM.delete v k
        IsRelated k v -> STM.lookup v k
        GetRelated k -> ListT.fold (pure <<$>> flip insert) mempty . STM.listTByKey k
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
