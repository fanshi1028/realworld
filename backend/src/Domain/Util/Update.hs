{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module Domain.Util.Update (WithUpdate, applyPatch) where

import Data.Generic.HKD (Construct, HKD, construct, deconstruct)
import qualified Data.Semigroup as SG
import Domain.Util.Validation (WithValidation)
import Relude.Extra (un)
import Validation (Validation (Failure))

type WithUpdate a = HKD (HKD (HKD a WithValidation) SG.Last) Maybe

applyPatch ::
  ( Construct WithValidation (r "all"),
    Construct SG.Last (HKD (r "all") WithValidation),
    Construct Maybe (HKD (HKD (r "all") WithValidation) SG.Last),
    Semigroup (WithUpdate (r "all")),
    Coercible (WithUpdate (r "all")) (r "update")
  ) =>
  r "update" ->
  r "all" ->
  WithValidation (r "all")
applyPatch update orig =
  let orig' = deconstruct $ deconstruct $ deconstruct orig
   in case construct $ orig' <> un update of
        Nothing -> Failure ("impossible: missing field!" :| [])
        Just h -> construct $ SG.getLast $ construct h
