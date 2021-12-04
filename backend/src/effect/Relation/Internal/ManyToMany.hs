{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
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
module Relation.Internal.ManyToMany where

import Control.Algebra (Algebra)
import Control.Carrier.NonDet.Church (runNonDetM)
import Control.Effect.Lift (Lift)
import Control.Effect.NonDet (oneOf)
import Control.Effect.Sum (Member)
import GHC.Base (Symbol)
import Relation.ToMany
  ( ToMany
      ( ToManyKey,
        ToManyValue,
        getRelatedToMany,
        isRelatedToMany,
        relateToMany,
        unrelateByKeyToMany,
        unrelateToMany
      ),
    ToManyRelationE,
  )

-- | @since 0.3.0.0
type ManyToManyRelationE (label :: Symbol) sig =
  ( ManyToMany label,
    ToManyRelationE (ManyLeft label) sig,
    ToManyRelationE (ManyRight label) sig,
    Member (Lift STM) sig,
    ToManyKey (ManyLeft label) ~ ToManyValue (ManyRight label),
    ToManyValue (ManyLeft label) ~ ToManyKey (ManyRight label)
  )

-- | @since 0.3.0.0
class ManyToMany (label :: Symbol) where
  -- | @since 0.3.0.0
  type ManyLeft label :: Symbol

  -- | @since 0.3.0.0
  type ManyRight label :: Symbol

  -- | @since 0.3.0.0
  relateManyToMany ::
    ( ManyToManyRelationE label sig,
      Algebra sig m
    ) =>
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
  unrelateByKeyLeftManyToMany ::
    (ManyToManyRelationE label sig, Algebra sig m) =>
    ToManyKey (ManyLeft label) ->
    m ()
  unrelateByKeyLeftManyToMany r1 = runNonDetM id $ do
    getRelatedToMany @(ManyLeft label) r1
      >>= oneOf
      >>= \r2 -> unrelateToMany @(ManyRight label) r2 r1
    unrelateByKeyToMany @(ManyLeft label) r1
  {-# INLINE unrelateByKeyLeftManyToMany #-}

  -- | @since 0.3.0.0
  unrelateByKeyRightManyToMany ::
    (ManyToManyRelationE label sig, Algebra sig m) =>
    ToManyKey (ManyRight label) ->
    m ()
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
  getRelatedLeftManyToMany ::
    (ManyToManyRelationE label sig, Algebra sig m) =>
    ToManyKey (ManyLeft label) ->
    m [ToManyValue (ManyLeft label)]
  getRelatedLeftManyToMany = getRelatedToMany @(ManyLeft label)
  {-# INLINE getRelatedLeftManyToMany #-}

  -- | @since 0.3.0.0
  getRelatedRightManyToMany ::
    (ManyToManyRelationE label sig, Algebra sig m) =>
    ToManyKey (ManyRight label) ->
    m [ToManyValue (ManyRight label)]
  getRelatedRightManyToMany = getRelatedToMany @(ManyRight label)
  {-# INLINE getRelatedRightManyToMany #-}
