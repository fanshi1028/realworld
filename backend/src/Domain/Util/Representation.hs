{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
module Domain.Util.Representation where

import Data.Generic.HKD (Construct (construct, deconstruct), HKD)
import qualified Data.Semigroup as SG
import GHC.TypeLits (Symbol)
import Relude.Extra (un)
import Validation (Validation (Failure))
import Validation.Carrier.Selective (WithUpdate, WithValidation)

class Transform (r :: Symbol -> Type) s1 s2 where
  transform :: r s1 -> r s2

class TransformM (m :: Type -> Type) (r :: Symbol -> Type) s1 s2 where
  transformM :: r s1 -> m (r s2)

instance Transform r s1 s2 => TransformM Identity (r :: Symbol -> Type) s1 s2 where
  transformM = Identity . transform

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
