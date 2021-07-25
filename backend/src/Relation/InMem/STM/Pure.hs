{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Relation.InMem.STM.Pure where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import GHC.TypeLits (Symbol)
import Relation.InMem.STM (E (IsRelated, Relate, Unrelate))
import qualified StmContainers.Multimap as STM

newtype C (r1 :: Symbol -> Type) (r2 :: Symbol -> Type) (m :: Type -> Type) a = C
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
  Algebra (E r1 r2 :+: sig) (C r1 r2 m)
  where
  alg _ (L action) ctx =
    ask >>= \db ->
      pure $
        case action of
          Relate -> \r1 r2 -> STM.insert r2 r1 db
          Unrelate -> \r1 r2 -> STM.delete r2 r1 db
          IsRelated -> \r1 r2 -> STM.lookup r2 r1 db
          <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
