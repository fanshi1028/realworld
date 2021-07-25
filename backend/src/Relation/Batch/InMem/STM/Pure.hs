{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.Batch.InMem.STM.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Data.HashSet (insert)
import GHC.TypeLits (Symbol)
import qualified ListT
import Relation.Batch.InMem.STM (E (GetRelated))
import qualified StmContainers.Multimap as STM (Multimap, lookupByKey)
import qualified StmContainers.Set as STM (listT)

newtype C (r1 :: Symbol -> Type) (r2 :: Symbol -> Type) (f :: Type -> Type) (m :: Type -> Type) a = C
  { run :: ReaderT (STM.Multimap (r1 "id") (r2 "id")) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader (STM.Multimap (r1 "id") (r2 "id")))

instance
  ( Algebra sig m,
    Eq (r1 "id"),
    Eq (r2 "id"),
    Hashable (r1 "id"),
    Hashable (r2 "id")
  ) =>
  Algebra (E r1 r2 HashSet :+: sig) (C r1 r2 HashSet m)
  where
  alg _ (L GetRelated) ctx =
    ask >>= \db ->
      pure $
        ( flip STM.lookupByKey db
            >=> maybe
              (pure mempty)
              (ListT.fold (\acc i -> pure $ insert i acc) mempty . STM.listT)
        )
          <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
