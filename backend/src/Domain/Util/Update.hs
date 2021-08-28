{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Copyright   : (c) fanshi1028 , 2021
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Not only updationg the domain data but also validating the updated value
--
-- @since 0.1.0.0
module Domain.Util.Update (WithUpdate, applyPatch) where

import Data.Generic.HKD (Construct, HKD, construct, deconstruct)
import qualified Data.Semigroup as SG
import Domain.Util.Validation (WithValidation)
import Relude.Extra (un)
import Validation (Validation (Failure))

-- | Convience type synonym, represent a patch to update a value using nested construct of "Data.Generic.HKD"
--
-- @since 0.1.0.0
type WithUpdate a = HKD (HKD (HKD a WithValidation) SG.Last) Maybe

-- | Apply an update patch, then validate the new result.
--
-- @since 0.1.0.0
applyPatch ::
  ( Construct WithValidation (r "all"),
    Construct SG.Last (HKD (r "all") WithValidation),
    Construct Maybe (HKD (HKD (r "all") WithValidation) SG.Last),
    Semigroup (WithUpdate (r "all")),
    Coercible (WithUpdate (r "all")) (r "update")
  ) =>
  -- | update patch
  r "update" ->
  -- | original value
  r "all" ->
  WithValidation (r "all")
applyPatch update orig =
  let orig' = deconstruct $ deconstruct $ deconstruct orig
   in case construct $ orig' <> un update of
        Nothing -> Failure ("impossible: missing field!" :| [])
        Just h -> construct $ SG.getLast $ construct h
