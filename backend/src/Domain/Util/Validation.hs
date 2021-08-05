{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module Domain.Util.Validation where

import Data.Aeson (FromJSON (parseJSON), defaultOptions, genericParseJSON, withArray)
import Data.Generic.HKD (HKD)
import qualified Data.HashSet as HS (fromList)
import qualified Data.Semigroup as SG
import qualified Validation as V (Validation (Success), failure)
import Domain.Util.Error (ValidationErr)
import Data.Time (UTCTime)

-- | TEMP: dangerous orphan instance
instance FromJSON (WithValidation a) => FromJSON (WithValidation [a]) where
  parseJSON = withArray "array" $ foldMapA parseJSON

-- instance FromJSON (WithValidation a) => FromJSON (WithValidation (Maybe a)) where
--   parseJSON = parseJSON

instance (Eq r, Hashable r, FromJSON (WithValidation r)) => FromJSON (WithValidation (HashSet r)) where
  parseJSON = fmap HS.fromList . sequenceA <<$>> parseJSON

deriving via NoValidation instance FromJSON (WithValidation Text)

instance FromJSON (WithValidation Bool) where
  parseJSON = pure <<$>> parseJSON

instance FromJSON (WithValidation Natural) where
  parseJSON = pure <<$>> parseJSON

instance FromJSON (WithValidation UTCTime) where
  parseJSON = pure <<$>> parseJSON

-- | NOTE: helper function for validation
validate :: (a -> Bool) -> e -> a -> V.Validation (NonEmpty e) a
validate p err raw = if p raw then V.Success raw else V.failure err

type WithValidation = V.Validation ValidationErr

newtype NoValidation' = NoValidation' Text deriving (Generic)

type NoValidation = WithValidation NoValidation'

instance FromJSON (WithValidation NoValidation') where
  parseJSON = pure <<$>> genericParseJSON defaultOptions

type WithUpdate a = HKD (HKD (HKD a WithValidation) SG.Last) Maybe
