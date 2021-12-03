{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : Helper
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Helpers for store-in-memory effect
--
-- @since 0.3.0.0
module Storage.InMem
  ( -- * Set storage in memory effect helper
    setInMemIsMember,
    setInMemGetAll,
    setInMemInsert,
    setInMemDelete,

    -- * Map storage in memory effect helper
    mapInMemGetAll,
    mapInMemGetById,
    mapInMemInsert,
    mapInMemDeleteById,
    mapInMemUpdateById,
  )
where

import Control.Algebra (Algebra)
import Control.Effect.Error (Throw, throwError)
import Control.Effect.Lift (Lift, sendM)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Domain.Transform (Transform, transform)
import qualified Focus as FC (Change (Leave, Remove), cases)
import qualified ListT (fold, toList)
import qualified StmContainers.Map as STMMap (Map, delete, focus, insert, listT, lookup)
import qualified StmContainers.Set as STMSet (Set, focus, insert, listT, lookup)
import Storage.Error (AlreadyExists (AlreadyExists), NotFound (NotFound))

setInMemIsMember ::
  ( Member (Lift STM) sig,
    Algebra sig m,
    Eq item,
    Hashable item,
    Member (R.Reader (STMSet.Set item)) sig
  ) =>
  item ->
  m Bool
setInMemIsMember e = R.ask >>= sendM . STMSet.lookup e

setInMemGetAll :: (Algebra sig m, Member (Lift STM) sig, Member (R.Reader (STMSet.Set item)) sig) => m [item]
setInMemGetAll = R.ask >>= sendM . ListT.fold (\r e -> pure $ e : r) [] . STMSet.listT

setInMemInsert ::
  ( Member (Lift STM) sig,
    Member (R.Reader (STMSet.Set item)) sig,
    Algebra sig m,
    Eq item,
    Hashable item
  ) =>
  item ->
  m ()
setInMemInsert e = R.ask >>= sendM . STMSet.insert e

setInMemDelete ::
  ( Algebra sig m,
    Hashable item,
    Eq item,
    Member (R.Reader (STMSet.Set item)) sig,
    Member (Throw (NotFound item)) sig,
    Member (Lift STM) sig
  ) =>
  item ->
  m ()
setInMemDelete e =
  R.ask
    >>= sendM . STMSet.focus (FC.cases (Nothing, FC.Leave) (const (Just (), FC.Remove))) e
    >>= maybe (throwError $ NotFound e) pure

_try ::
  ( Algebra sig m,
    Hashable a,
    Eq a,
    Member (Throw (NotFound a)) sig,
    Member (Lift STM) sig
  ) =>
  (value -> (Maybe c, FC.Change value)) ->
  a ->
  STMMap.Map a value ->
  m c
_try action id' = sendM . STMMap.focus (FC.cases (Nothing, FC.Leave) action) id' >=> maybe (throwError $ NotFound id') pure

mapInMemGetAll :: (Algebra sig m, Member (Lift STM) sig, Member (R.Reader (STMMap.Map key v)) sig) => m [(key, v)]
mapInMemGetAll = R.ask >>= sendM . ListT.toList . STMMap.listT

mapInMemGetById ::
  ( Algebra sig m,
    Hashable key,
    Eq key,
    Member (R.Reader (STMMap.Map key v)) sig,
    Member (Throw (NotFound key)) sig,
    Member (Lift STM) sig
  ) =>
  key ->
  m v
mapInMemGetById id' =
  R.ask
    >>= sendM . STMMap.lookup id'
    >>= maybe (throwError $ NotFound id') pure

mapInMemInsert :: (Member (Lift STM) sig, Algebra sig m, Eq key, Hashable key, Member (R.Reader (STMMap.Map key value)) sig) => key -> value -> m ()
mapInMemInsert key value = R.ask >>= sendM . STMMap.insert value key

mapInMemDeleteById ::
  ( Hashable k,
    Algebra sig m,
    Eq k,
    Member (Throw (NotFound k)) sig,
    Member (Lift STM) sig
  ) =>
  k ->
  STMMap.Map k v ->
  m ()
mapInMemDeleteById = _try $ const (Just (), FC.Remove)

mapInMemUpdateById ::
  ( Algebra sig m,
    Hashable key,
    Eq key,
    Member (R.Reader (STMMap.Map key v)) sig,
    Transform v key,
    Member (Lift STM) sig,
    Member (Throw (AlreadyExists key)) sig,
    Member (Throw (NotFound key)) sig
  ) =>
  key ->
  (v -> v) ->
  m v
mapInMemUpdateById id' updateF = do
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
