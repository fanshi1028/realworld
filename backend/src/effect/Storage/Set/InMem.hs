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
module Storage.Set.InMem where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Control.Effect.Error (Throw, throwError)
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import qualified Focus as FC (Change (Leave, Remove), cases)
import qualified ListT (fold)
import qualified StmContainers.Set as STM (Set, focus, insert, listT, lookup)
import Storage.Set (E (Delete, GetAll, Insert, IsMember))
import Util.Error (NotFound (NotFound))

-- | @since 0.1.0.0
newtype C (e :: Type) m a = C
  { -- | @since 0.1.0.0
    run' :: ReaderT (STM.Set e) m a
  }
  deriving (Functor, Applicative, Monad, MonadReader (STM.Set e))

-- | @since 0.1.0.0
instance
  ( Member (Lift STM) sig,
    Member (Throw (NotFound e)) sig,
    Algebra sig m,
    Eq e,
    Hashable e
  ) =>
  Algebra (E e :+: sig) (C e m)
  where
  alg hdl sig ctx = case sig of
    L action ->
      (<$ ctx) <$> do
        ask >>= case action of
          IsMember e -> sendM . STM.lookup e
          GetAll -> sendM . _getAll
          Insert e -> sendM . STM.insert e
          Delete e -> _delete e
    R other -> C $ alg (run' . hdl) (R other) ctx
    where
      _getAll = ListT.fold (\r e -> pure $ e : r) [] . STM.listT
      _delete e =
        sendM . STM.focus (FC.cases (Nothing, FC.Leave) (const (Just (), FC.Remove))) e
          >=> maybe (throwError $ NotFound e) pure
  {-# INLINE alg #-}

-- | @since 0.1.0.0
run ::
  -- | the in-memory db
  STM.Set e ->
  -- | effect stack
  C e m a ->
  m a
run db = run' >>> usingReaderT db