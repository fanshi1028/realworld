{-# LANGUAGE AllowAmbiguousTypes #-}
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
-- ToOne Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ToOne where

import Control.Algebra (Algebra)
import Control.Carrier.Lift (sendM)
import Control.Effect.Labelled (LabelledMember)
import Control.Effect.Lift (Lift)
import qualified Control.Effect.Reader.Labelled as R (Reader, ask)
import Control.Effect.Sum (Member)
import qualified Focus (update)
import GHC.Base (Symbol)
import qualified StmContainers.Map as STMMap

-- | @since 0.3.0.0
type ToOneRelationE (label :: Symbol) sig =
  ( LabelledMember label (R.Reader (STMMap.Map (ToOneKey label) (ToOneValue label))) sig,
    Member (Lift STM) sig
  )

-- | @since 0.3.0.0
class (Eq (ToOneKey label), Eq (ToOneValue label), Hashable (ToOneKey label)) => ToOne (label :: Symbol) where
  -- | @since 0.3.0.0
  type ToOneKey label

  -- | @since 0.3.0.0
  type ToOneValue label

  -- | @since 0.3.0.0
  relateToOne :: (ToOneRelationE label sig, Algebra sig m) => ToOneKey label -> ToOneValue label -> m ()
  relateToOne key value = R.ask @label >>= sendM . STMMap.insert value key
  {-# INLINE relateToOne #-}

  -- | @since 0.3.0.0
  unrelateToOne :: (ToOneRelationE label sig, Algebra sig m) => ToOneKey label -> ToOneValue label -> m ()
  unrelateToOne key value = R.ask @label >>= sendM . STMMap.focus (Focus.update (\v -> if v == value then Nothing else Just v)) key
  {-# INLINE unrelateToOne #-}

  -- | @since 0.3.0.0
  getRelatedToOne :: (ToOneRelationE label sig, Algebra sig m) => ToOneKey label -> m (Maybe (ToOneValue label))
  getRelatedToOne r1 = R.ask @label >>= sendM . STMMap.lookup r1
  {-# INLINE getRelatedToOne #-}