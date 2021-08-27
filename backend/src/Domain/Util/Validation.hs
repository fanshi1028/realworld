{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module Domain.Util.Validation (validate, WithValidation, NoValidation (..), WithNoValidation) where

import Data.Aeson (FromJSON (parseJSON), withArray)
import qualified Data.HashSet as HS (fromList)
import Data.Time (UTCTime)
import Domain.Util.Error (ValidationErr)
import Servant (FromHttpApiData (parseQueryParam))
import qualified Validation as V (Validation (Success), failure)

-- | TEMP: dangerous orphan instance
instance FromJSON (WithValidation a) => FromJSON (WithValidation [a]) where
  parseJSON = withArray "array" $ foldMapA parseJSON

instance (Eq r, Hashable r, FromJSON (WithValidation r)) => FromJSON (WithValidation (HashSet r)) where
  parseJSON = fmap HS.fromList . sequenceA <<$>> parseJSON

deriving via (WithNoValidation Text) instance FromJSON (WithValidation Text)

deriving via (WithNoValidation UTCTime) instance FromJSON (WithValidation UTCTime)

-- instance FromJSON (WithValidation a) => FromJSON (WithValidation (Maybe a)) where
--   parseJSON = parseJSON

-- | NOTE: helper function for validation
validate :: (a -> Bool) -> e -> a -> V.Validation (NonEmpty e) a
validate p err raw = if p raw then V.Success raw else V.failure err

type WithValidation = V.Validation ValidationErr

newtype NoValidation a = NoValidation a deriving (Generic, FromHttpApiData, FromJSON)

type WithNoValidation a = WithValidation (NoValidation a)

instance FromJSON a => FromJSON (WithNoValidation a) where
  parseJSON = pure <<$>> parseJSON

instance FromHttpApiData a => FromHttpApiData (WithNoValidation a) where
  parseQueryParam = pure <<$>> parseQueryParam

