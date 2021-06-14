{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module Validation.Adaptor where

import Data.Aeson (FromJSON (parseJSON), defaultOptions, genericParseJSON, withArray)
import Data.Generic.HKD (HKD)
import qualified Data.Semigroup as SG
import qualified Validation as V (Validation (Success), failure)

-- | TEMP: dangerous orphan instance
instance FromJSON (WithValidation a) => FromJSON (WithValidation [a]) where
  parseJSON = withArray "array" $ foldMapA parseJSON

deriving via NoValidation instance FromJSON (WithValidation Text)

-- | NOTE: helper function for validation
validate :: (a -> Bool) -> e -> a -> V.Validation (NonEmpty e) a
validate p err raw = if p raw then V.Success raw else V.failure err

type WithValidation = V.Validation (NonEmpty Text)

newtype NoValidation' = NoValidation' Text deriving (Generic)

type NoValidation = WithValidation NoValidation'

instance FromJSON (WithValidation NoValidation') where
  parseJSON = pure <<$>> genericParseJSON defaultOptions

type WithUpdate a = HKD (HKD (HKD a WithValidation) SG.Last) Maybe

-- class Validator a where
--   validators :: [a -> Bool]
