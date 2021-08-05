{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.InMem.OneToMany where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import GHC.TypeLits (Symbol)
import Relation (E (IsRelated, Relate, Unrelate))
import qualified StmContainers.Multimap as STM

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
        Relate _ k v -> STM.insert v k
        Unrelate _ k v -> STM.delete v k
        IsRelated _ k v -> STM.lookup v k
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
