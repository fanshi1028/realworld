{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Helper
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Helpers for store-in-memory effect
--
-- @since 0.3.0.0
module InMem.Storage
  ( -- * Set storage in memory effect

    -- ** Helper
    isMemberSetInMem,
    getAllSetInMem,
    insertSetInMem,
    deleteSetInMem,

    -- ** Constraint
    SetInMemE,

    -- * Map storage in memory effect

    -- ** Type alias
    TableInMem,

    -- ** Constraint
    MapInMemE',
    MapInMemE,

    -- ** Helper
    getAllMapInMem,
    getByIdMapInMem,
    insertMapInMem,
    deleteByIdMapInMem,
    updateByIdMapInMem,
  )
where

import Control.Algebra (Algebra)
import Control.Effect.Error (Throw, throwError)
import Control.Effect.Lift (Lift, sendM)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Domain (Domain)
import Domain.Transform (Transform, transform)
import Storage.Error (AlreadyExists (AlreadyExists), NotFound (NotFound))
import qualified Focus as FC (Change (Leave, Remove), cases)
import qualified ListT (fold, toList)
import qualified StmContainers.Map as STMMap (Map, delete, focus, insert, listT, lookup)
import qualified StmContainers.Set as STMSet (Set, focus, insert, listT, lookup)
import Storage.Map (HasStorage (ContentOf), IdOf)

-- | @since 0.3.0.0
isMemberSetInMem ::
  ( Eq item,
    Hashable item,
    Algebra sig m,
    Member (Lift STM) sig,
    Member (R.Reader (STMSet.Set item)) sig
  ) =>
  item ->
  m Bool
isMemberSetInMem e = R.ask >>= sendM . STMSet.lookup e
{-# INLINE isMemberSetInMem #-}

-- | @since 0.3.0.0
getAllSetInMem :: (Algebra sig m, Member (Lift STM) sig, Member (R.Reader (STMSet.Set item)) sig) => m [item]
getAllSetInMem = R.ask >>= sendM . ListT.fold (\r e -> pure $ e : r) [] . STMSet.listT
{-# INLINE getAllSetInMem #-}

-- | @since 0.3.0.0
insertSetInMem ::
  ( Eq item,
    Hashable item,
    Member (Lift STM) sig,
    Member (R.Reader (STMSet.Set item)) sig,
    Algebra sig m
  ) =>
  item ->
  m ()
insertSetInMem e = R.ask >>= sendM . STMSet.insert e
{-# INLINE insertSetInMem #-}

-- | @since 0.3.0.0
deleteSetInMem ::
  ( Eq item,
    Hashable item,
    Algebra sig m,
    Member (R.Reader (STMSet.Set item)) sig,
    Member (Throw (NotFound item)) sig,
    Member (Lift STM) sig
  ) =>
  item ->
  m ()
deleteSetInMem e =
  R.ask
    >>= sendM . STMSet.focus (FC.cases (Nothing, FC.Leave) (const (Just (), FC.Remove))) e
    >>= maybe (throwError $ NotFound e) pure
{-# INLINE deleteSetInMem #-}

-- | @since 0.3.0.0
type SetInMemE item sig =
  ( Eq item,
    Hashable item,
    Member (R.Reader (STMSet.Set item)) sig,
    Member (Throw (NotFound item)) sig,
    Member (Lift STM) sig
  )

-- | @since 0.3.0.0
type TableInMem s = STMMap.Map (IdOf s) (ContentOf s)

-- | @since 0.3.0.0
getAllMapInMem ::
  ( Member (R.Reader (TableInMem d)) sig,
    Algebra sig m,
    Member (Lift STM) sig
  ) =>
  m [(IdOf d, ContentOf d)]
getAllMapInMem = R.ask >>= sendM . ListT.toList . STMMap.listT
{-# INLINE getAllMapInMem #-}

-- | @since 0.3.0.0
getByIdMapInMem ::
  ( Hashable (IdOf d),
    Eq (IdOf d),
    Algebra sig m,
    Member (R.Reader (TableInMem d)) sig,
    Member (Throw (NotFound (IdOf d))) sig,
    Member (Lift STM) sig
  ) =>
  IdOf d ->
  m (ContentOf d)
getByIdMapInMem id' =
  R.ask
    >>= sendM . STMMap.lookup id'
    >>= maybe (throwError $ NotFound id') pure
{-# INLINE getByIdMapInMem #-}

-- | @since 0.3.0.0
insertMapInMem ::
  ( Eq (IdOf d),
    Hashable (IdOf d),
    Algebra sig m,
    Member (Lift STM) sig,
    Member (R.Reader (TableInMem d)) sig
  ) =>
  IdOf d ->
  ContentOf d ->
  m ()
insertMapInMem key value = R.ask >>= sendM . STMMap.insert value key
{-# INLINE insertMapInMem #-}

-- | @since 0.3.0.0
deleteByIdMapInMem ::
  ( Hashable (IdOf d),
    Eq (IdOf d),
    Algebra sig m,
    Member (Throw (NotFound (IdOf d))) sig,
    Member (Lift STM) sig,
    Member (R.Reader (TableInMem d)) sig
  ) =>
  IdOf d ->
  m (ContentOf d)
deleteByIdMapInMem id' =
  R.ask
    >>= sendM . STMMap.focus (FC.cases (Nothing, FC.Leave) (\v -> (Just v, FC.Remove))) id'
    >>= maybe (throwError $ NotFound id') pure
{-# INLINE deleteByIdMapInMem #-}

-- | @since 0.3.0.0
updateByIdMapInMem ::
  ( Hashable (IdOf d),
    Eq (IdOf d),
    Transform (ContentOf d) (IdOf d),
    Algebra sig m,
    Member (R.Reader (TableInMem d)) sig,
    Member (Lift STM) sig,
    Member (Throw (AlreadyExists (IdOf d))) sig,
    Member (Throw (NotFound (IdOf d))) sig
  ) =>
  IdOf d ->
  (ContentOf d -> ContentOf d) ->
  m (ContentOf d)
updateByIdMapInMem id' updateF = do
  stmMap <- R.ask
  sendM (STMMap.lookup id' stmMap)
    >>= \case
      Nothing -> throwError $ NotFound id'
      Just ele ->
        let new = updateF ele
            newId = transform new
         in new
              <$ if newId == id'
                then sendM $ STMMap.insert new newId stmMap
                else
                  sendM (STMMap.lookup newId stmMap) >>= \case
                    Just _ -> throwError $ AlreadyExists newId
                    Nothing -> sendM $ STMMap.delete id' stmMap >> STMMap.insert new newId stmMap
{-# INLINE updateByIdMapInMem #-}

-- | @since 0.3.0.0
type MapInMemE' key value sig =
  ( Eq key,
    Hashable key,
    Transform value key,
    Member (Lift STM) sig,
    Member (R.Reader (STMMap.Map key value)) sig,
    Member (Throw (AlreadyExists key)) sig,
    Member (Throw (NotFound key)) sig
  )

-- | @since 0.3.0.0
type MapInMemE (d :: Domain) sig = MapInMemE' (IdOf d) (ContentOf d) sig
