{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.OneToMany.InMem where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import GHC.TypeLits (Symbol)
import qualified ListT (toList)
import Relation.OneToMany (E (GetRelated, IsRelated, Relate, Unrelate, UnrelateByKey))
import qualified StmContainers.Multimap as STM (Multimap, delete, insert, listTByKey, lookup, deleteByKey)

newtype
  C
    (r1 :: Type)
    (r :: Symbol)
    (r2 :: Type)
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
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
