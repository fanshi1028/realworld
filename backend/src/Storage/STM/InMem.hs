{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Storage.STM.InMem where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import qualified Focus as FC (Change (Leave, Remove, Set), cases)
import GHC.TypeLits (Symbol)
import qualified ListT (fold)
import qualified StmContainers.Map as STM (focus, insert, listT, lookup)
import Storage.InMem (TableInMem)
import Storage.STM (E (DeleteById, GetAll, GetById, Insert, UpdateById))

newtype C (r :: Symbol -> Type) m a = C
  { run :: ReaderT (TableInMem r) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader (TableInMem r))

instance
  ( Eq (r "id"),
    Show (r "id"),
    Hashable (r "id"),
    Algebra sig m
  ) =>
  Algebra (E r :+: sig) (C r m)
  where
  alg hdl sig ctx = case sig of
    (L action) ->
      asks $
        (<$ ctx)
          . ( case action of
                GetById -> flip _getById
                GetAll -> _getAll
                Insert -> \db key value -> STM.insert value key db >>= const (pure value)
                UpdateById -> \db id' updateF -> join $ STM.focus (_lookupAndUpdate id' updateF) id' db
                DeleteById -> \db id' -> join $ STM.focus (_lookupAndDelete id') id' db
            )
    (R other) -> C $ alg (run . hdl) (R other) ctx
    where
      _notFound id' = throwSTM $ show @Text id' <> "not found" :| []
      _getById id' = STM.lookup id'
      _getAll = ListT.fold (\r (_, v) -> pure $ v : r) [] . STM.listT
      _lookupAndUpdate id' updateF =
        FC.cases (_notFound id', FC.Leave) (\ele -> let new = updateF ele in (pure new, FC.Set new))
      _lookupAndDelete id' = FC.cases (_notFound id', FC.Leave) (const (pure (), FC.Remove))
