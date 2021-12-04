{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Helpers to run Relation effect in memory
--
-- @since 0.3.0.0
module Relation.InMem
  ( -- * ToOne

    -- ** Constriant
    ToOneRelationE,

    -- ** Helpers
    relateToOne,
    unrelateToOne,
    getRelatedToOne,
  )
where

import Control.Algebra (Algebra)
import Control.Effect.Labelled (LabelledMember)
import Control.Effect.Lift (Lift, sendM)
import qualified Control.Effect.Reader.Labelled as R
import Control.Effect.Sum (Member)
import qualified Focus (update)
import GHC.Base (Symbol)
import Relation.ToOne (ToOne (ToOneKey, ToOneValue))
import qualified StmContainers.Map as STMMap (Map, focus, insert, lookup)

-- | @since 0.3.0.0
relateToOne ::
  forall (label :: Symbol) m sig.
  ( LabelledMember label (R.Reader (STMMap.Map (ToOneKey label) (ToOneValue label))) sig,
    Eq (ToOneKey label),
    Hashable (ToOneKey label),
    Member (Lift STM) sig,
    Algebra sig m
  ) =>
  ToOneKey label ->
  ToOneValue label ->
  m ()
relateToOne key value = R.ask @label >>= sendM . STMMap.insert value key
{-# INLINE relateToOne #-}

-- | @since 0.3.0.0
unrelateToOne ::
  forall (label :: Symbol) m sig.
  ( LabelledMember label (R.Reader (STMMap.Map (ToOneKey label) (ToOneValue label))) sig,
    Member (Lift STM) sig,
    Algebra sig m,
    Hashable (ToOneKey label),
    Eq (ToOneKey label),
    Eq (ToOneValue label)
  ) =>
  ToOneKey label ->
  ToOneValue label ->
  m ()
unrelateToOne key value = R.ask @label >>= sendM . STMMap.focus (Focus.update (\v -> if v == value then Nothing else Just v)) key
{-# INLINE unrelateToOne #-}

-- | @since 0.3.0.0
getRelatedToOne ::
  forall (label :: Symbol) m sig.
  ( LabelledMember label (R.Reader (STMMap.Map (ToOneKey label) (ToOneValue label))) sig,
    Member (Lift STM) sig,
    Algebra sig m,
    Eq (ToOneKey label),
    Hashable (ToOneKey label)
  ) =>
  ToOneKey label ->
  m (Maybe (ToOneValue label))
getRelatedToOne r1 = R.ask @label >>= sendM . STMMap.lookup r1
{-# INLINE getRelatedToOne #-}

type ToOneRelationE (label :: Symbol) sig =
  ( LabelledMember label (R.Reader (STMMap.Map (ToOneKey label) (ToOneValue label))) sig,
    Eq (ToOneKey label),
    Hashable (ToOneKey label),
    Eq (ToOneValue label)
  )
