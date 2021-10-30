{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier to store in memory
--
-- @since 0.1.0.0
module Storage.Map.InMem where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Error (Throw, throwError)
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import Domain.Transform (Transform (transform))
import qualified Focus as FC (Change (Leave, Remove), cases)
import qualified ListT (fold)
import qualified StmContainers.Map as STM (Map, delete, focus, insert, listT, lookup)
import Storage.Map (AlreadyExists (AlreadyExists), E (DeleteById, GetAll, GetById, Insert, UpdateById), HasStorage (ContentOf, IdOf))
import Util.Error (NotFound (NotFound))

-- | @since 0.2.0.0
type TableInMem' k v = STM.Map k v

-- | @since 0.2.0.0
type TableInMem s = STM.Map (IdOf s) (ContentOf s)

-- | @since 0.2.0.0
newtype C s m a = C
  { -- | @since 0.2.0.0
    run' :: ReaderT (TableInMem s) m a
  }
  deriving (Functor, Applicative, Monad, MonadReader (TableInMem s))

-- | @since 0.2.0.0
instance
  ( Show (IdOf s),
    Eq (IdOf s),
    Hashable (IdOf s),
    Member (Lift STM) sig,
    Member (Throw (NotFound (IdOf s))) sig,
    Member (Throw (AlreadyExists (IdOf s))) sig,
    Transform (ContentOf s) (IdOf s),
    Algebra sig m
  ) =>
  Algebra (E s :+: sig) (C s m)
  where
  alg hdl sig ctx = case sig of
    L action ->
      (<$ ctx) <$> do
        ask >>= case action of
          GetById id' ->
            sendM . STM.lookup id'
              >=> maybe (throwError $ NotFound id') pure
          GetAll -> sendM . _getAll
          Insert key value -> sendM . STM.insert value key
          UpdateById id' updateF -> _tryUpdate updateF id'
          DeleteById id' -> _tryDelete id'
    R other -> C $ alg (run' . hdl) (R other) ctx
    where
      _getAll = ListT.fold (\r (_, v) -> pure $ v : r) [] . STM.listT
      _try action id' = sendM . STM.focus (FC.cases (Nothing, FC.Leave) action) id' >=> maybe (throwError $ NotFound id') pure
      _tryUpdate updateF id' stmMap =
        sendM (STM.lookup id' stmMap)
          >>= \case
            Nothing -> throwError $ NotFound id'
            Just ele ->
              let new = updateF ele
                  newId = transform new
               in new
                    <$ if newId == id'
                      then sendM $ STM.insert new newId stmMap
                      else
                        sendM (STM.lookup newId stmMap) >>= \case
                          Just _ -> throwError $ AlreadyExists newId
                          Nothing -> sendM $ STM.delete id' stmMap >> STM.insert new newId stmMap
      _tryDelete id' = _try (const (Just (), FC.Remove)) id'
  {-# INLINE alg #-}

-- | @since 0.1.0.0
run ::
  -- | the in-memory db
  TableInMem s ->
  -- | the effect stack
  C s m a ->
  m a
run db = run' >>> usingReaderT db
