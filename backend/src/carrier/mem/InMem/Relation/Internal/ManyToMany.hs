{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass & Instances
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- ManyToMany Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ManyToMany where

import Control.Algebra (Algebra)
import Control.Carrier.NonDet.Church (runNonDetM)
import Control.Effect.Lift (Lift)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import InMem.Relation.Internal.ToMany
  ( ToMany
      ( ToManyKey,
        ToManyValue,
        getAllKeyValueToMany,
        getRelatedToMany,
        isRelatedToMany,
        relateToMany,
        unrelateByKeyToMany,
        unrelateToMany
      ),
    ToManyRelationE,
    getAllKeyToMany,
  )

-- | @since 0.3.0.0
type ManyToManyRelationE label sig =
  ( ToManyRelationE (ManyLeft label) sig,
    ToManyRelationE (ManyRight label) sig,
    Member (Lift STM) sig
  )

-- | @since 0.3.0.0
class
  ( ToMany (ManyLeft label),
    ToMany (ManyRight label),
    ToManyKey (ManyLeft label) ~ ToManyValue (ManyRight label),
    ToManyValue (ManyLeft label) ~ ToManyKey (ManyRight label)
  ) =>
  ManyToMany label
  where
  -- | @since 0.3.0.0
  type ManyLeft label

  -- | @since 0.3.0.0
  type ManyRight label

  -- | @since 0.3.0.0
  relateManyToMany ::
    (ManyToManyRelationE label sig, Algebra sig m) =>
    ToManyKey (ManyLeft label) ->
    ToManyKey (ManyRight label) ->
    m ()
  relateManyToMany r1 r2 = do
    relateToMany @(ManyLeft label) r1 r2
    relateToMany @(ManyRight label) r2 r1
  {-# INLINE relateManyToMany #-}

  -- | @since 0.3.0.0
  unrelateManyToMany ::
    (ManyToManyRelationE label sig, Algebra sig m) =>
    ToManyKey (ManyLeft label) ->
    ToManyKey (ManyRight label) ->
    m ()
  unrelateManyToMany r1 r2 = do
    unrelateToMany @(ManyLeft label) r1 r2
    unrelateToMany @(ManyRight label) r2 r1
  {-# INLINE unrelateManyToMany #-}

  -- | @since 0.3.0.0
  unrelateByKeyLeftManyToMany :: (ManyToManyRelationE label sig, Algebra sig m) => ToManyKey (ManyLeft label) -> m ()
  unrelateByKeyLeftManyToMany r1 = runNonDetM id $ do
    getRelatedToMany @(ManyLeft label) r1
      >>= oneOf
      >>= \r2 -> unrelateToMany @(ManyRight label) r2 r1
    unrelateByKeyToMany @(ManyLeft label) r1
  {-# INLINE unrelateByKeyLeftManyToMany #-}

  -- | @since 0.3.0.0
  unrelateByKeyRightManyToMany :: (ManyToManyRelationE label sig, Algebra sig m) => ToManyKey (ManyRight label) -> m ()
  unrelateByKeyRightManyToMany r2 = runNonDetM id $ do
    getRelatedToMany @(ManyRight label) r2
      >>= oneOf
      >>= \r1 -> unrelateToMany @(ManyLeft label) r1 r2
    unrelateByKeyToMany @(ManyRight label) r2
  {-# INLINE unrelateByKeyRightManyToMany #-}

  -- | @since 0.3.0.0
  isRelatedManyToMany ::
    (ManyToManyRelationE label sig, Algebra sig m) =>
    ToManyKey (ManyLeft label) ->
    ToManyKey (ManyRight label) ->
    m Bool
  isRelatedManyToMany = isRelatedToMany @(ManyLeft label)
  {-# INLINE isRelatedManyToMany #-}

  -- | @since 0.3.0.0
  getRelatedLeftManyToMany :: (ManyToManyRelationE label sig, Algebra sig m) => ToManyKey (ManyLeft label) -> m [ToManyValue (ManyLeft label)]
  getRelatedLeftManyToMany = getRelatedToMany @(ManyLeft label)
  {-# INLINE getRelatedLeftManyToMany #-}

  -- | @since 0.3.0.0
  getRelatedRightManyToMany :: (ManyToManyRelationE label sig, Algebra sig m) => ToManyKey (ManyRight label) -> m [ToManyValue (ManyRight label)]
  getRelatedRightManyToMany = getRelatedToMany @(ManyRight label)
  {-# INLINE getRelatedRightManyToMany #-}

  -- | @since 0.4.0.0
  getAllLeftManyToMany :: (ManyToManyRelationE label sig, Algebra sig m) => m [ToManyKey (ManyLeft label)]
  getAllLeftManyToMany = getAllKeyToMany @(ManyLeft label)
  {-# INLINE getAllLeftManyToMany #-}

  -- | @since 0.4.0.0
  getAllRightManyToMany :: (ManyToManyRelationE label sig, Algebra sig m) => m [ToManyKey (ManyRight label)]
  getAllRightManyToMany = getAllKeyToMany @(ManyRight label)
  {-# INLINE getAllRightManyToMany #-}
