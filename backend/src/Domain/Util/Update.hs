{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Not only updationg the domain data but also validating the updated value
--
-- @since 0.1.0.0
module Domain.Util.Update (WithUpdate, applyPatch) where

import Data.Generic.HKD (Construct, HKD, construct, deconstruct)
import qualified Data.Semigroup as SG
import Relude.Extra (un)

-- | Convenient type synonym, represent a patch to update a value using nested construct of "Data.Generic.HKD"
--
-- @since 0.2.0.0
type WithUpdate a = HKD (HKD a SG.Last) Maybe

-- | Apply an update patch, then validate the new result.
--
-- @since 0.2.0.0
applyPatch ::
  ( Construct SG.Last (r "all"),
    Construct Maybe (HKD (r "all") SG.Last),
    Semigroup (WithUpdate (r "all")),
    Coercible (WithUpdate (r "all")) (r "update")
  ) =>
  -- | update patch
  r "update" ->
  -- | original value
  r "all" ->
  r "all"
applyPatch update orig =
  let orig' = deconstruct $ deconstruct orig
   in case construct $ orig' <> un update of
        Nothing -> error "impossible: missing field!"
        Just h -> SG.getLast $ construct h
