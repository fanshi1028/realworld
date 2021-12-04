{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass & Instances
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- ToMany Relation
--
-- @since 0.3.0.0
module Relation.Internal.ToMany where

import Control.Algebra (Algebra)
import Control.Carrier.Lift (sendM)
import Control.Effect.Labelled (LabelledMember)
import Control.Effect.Lift (Lift)
import qualified Control.Effect.Reader.Labelled as R (Reader, ask)
import Control.Effect.Sum (Member)
import GHC.Base (Symbol)
import qualified ListT (toList)
import qualified StmContainers.Multimap as STMMulti (Multimap, delete, deleteByKey, insert, listTByKey, lookup)

-- | @since 0.3.0.0
type ToManyRelationE (label :: Symbol) sig =
  ( ToMany label,
    LabelledMember label (R.Reader (STMMulti.Multimap (ToManyKey label) (ToManyValue label))) sig,
    Eq (ToManyKey label),
    Hashable (ToManyKey label),
    Eq (ToManyValue label)
  )

-- | @since 0.3.0.0
class ToMany (label :: Symbol) where
  -- | @since 0.3.0.0
  type ToManyKey label

  -- | @since 0.3.0.0
  type ToManyValue label

  -- | @since 0.3.0.0
  relateToMany ::
    ( LabelledMember label (R.Reader (STMMulti.Multimap (ToManyKey label) (ToManyValue label))) sig,
      Algebra sig m,
      Member (Lift STM) sig,
      Eq (ToManyKey label),
      Eq (ToManyValue label),
      Hashable (ToManyKey label),
      Hashable (ToManyValue label)
    ) =>
    ToManyKey label ->
    ToManyValue label ->
    m ()
  relateToMany k v = R.ask @label >>= sendM . STMMulti.insert v k
  {-# INLINE relateToMany #-}

  -- | @since 0.3.0.0
  unrelateToMany ::
    ( LabelledMember label (R.Reader (STMMulti.Multimap (ToManyKey label) (ToManyValue label))) sig,
      Algebra sig m,
      Member (Lift STM) sig,
      Eq (ToManyKey label),
      Eq (ToManyValue label),
      Hashable (ToManyKey label),
      Hashable (ToManyValue label)
    ) =>
    ToManyKey label ->
    ToManyValue label ->
    m ()
  unrelateToMany k v = R.ask @label >>= sendM . STMMulti.delete v k
  {-# INLINE unrelateToMany #-}

  -- | @since 0.3.0.0
  unrelateByKeyToMany ::
    ( LabelledMember label (R.Reader (STMMulti.Multimap (ToManyKey label) (ToManyValue label))) sig,
      Algebra sig m,
      Member (Lift STM) sig,
      Eq (ToManyKey label),
      Hashable (ToManyKey label)
    ) =>
    ToManyKey label ->
    m ()
  unrelateByKeyToMany k = R.ask @label >>= sendM . STMMulti.deleteByKey k
  {-# INLINE unrelateByKeyToMany #-}

  -- | @since 0.3.0.0
  isRelatedToMany ::
    ( LabelledMember label (R.Reader (STMMulti.Multimap (ToManyKey label) (ToManyValue label))) sig,
      Algebra sig m,
      Member (Lift STM) sig,
      Eq (ToManyKey label),
      Eq (ToManyValue label),
      Hashable (ToManyKey label),
      Hashable (ToManyValue label)
    ) =>
    ToManyKey label ->
    ToManyValue label ->
    m Bool
  isRelatedToMany k v = R.ask @label >>= sendM . STMMulti.lookup v k
  {-# INLINE isRelatedToMany #-}

  -- | @since 0.3.0.0
  getRelatedToMany ::
    ( LabelledMember label (R.Reader (STMMulti.Multimap (ToManyKey label) (ToManyValue label))) sig,
      Algebra sig m,
      Member (Lift STM) sig,
      Eq (ToManyKey label),
      Hashable (ToManyKey label)
    ) =>
    ToManyKey label ->
    m [ToManyValue label]
  getRelatedToMany k = R.ask @label >>= sendM . ListT.toList . STMMulti.listTByKey k
  {-# INLINE getRelatedToMany #-}
