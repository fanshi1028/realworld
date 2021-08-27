{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module Domain.Util.Update (WithUpdate, applyPatch) where

import Data.Generic.HKD (Construct, HKD, deconstruct, construct)
import qualified Data.Semigroup as SG
import Domain.Util.Validation (WithValidation)
import Relude.Extra (un)
import Validation (Validation(Failure))

type WithUpdate a = HKD (HKD (HKD a WithValidation) SG.Last) Maybe

type Patchable r =
  ( Coercible (WithUpdate (r "all")) (r "update"),
    Construct WithValidation (r "all"),
    Construct SG.Last (HKD (r "all") WithValidation),
    Construct Maybe (HKD (HKD (r "all") WithValidation) SG.Last),
    Semigroup (WithUpdate (r "all"))
  )

applyPatch ::
  (Patchable r) =>
  r "update" ->
  r "all" ->
  WithValidation (r "all")
applyPatch update orig =
  let orig' = deconstruct $ deconstruct $ deconstruct orig
   in case construct $ orig' <> un update of
        Nothing -> Failure ("impossible: missing field!" :| [])
        Just h -> construct $ SG.getLast $ construct h
