{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- When we get data from the outside world, we do validation.
-- So we integrate validation with instance like 'FromJSON', 'FromHttpApiData', etc
--
-- @since 0.1.0.0
module Domain.Util.Validation
  ( -- * Types
    WithValidation,
    NoValidation (..),
    WithNoValidation,

    -- * Helpers
    validate,
  )
where

import Data.Aeson (FromJSON (parseJSON), withArray)
import qualified Data.HashSet as HS (fromList)
import Data.Time (UTCTime)
import Domain.Util.Error (ValidationErr)
import Servant (FromHttpApiData (parseQueryParam))
import qualified Validation as V (Validation (Success), failure)

-- | @since 0.1.0.0
instance FromJSON (WithValidation a) => FromJSON (WithValidation [a]) where
  parseJSON = withArray "array" $ foldMapA parseJSON

-- | @since 0.1.0.0
instance (Eq r, Hashable r, FromJSON (WithValidation r)) => FromJSON (WithValidation (HashSet r)) where
  parseJSON = fmap HS.fromList . sequenceA <<$>> parseJSON

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Text)

-- | @since 0.1.0.0
deriving via (WithNoValidation UTCTime) instance FromJSON (WithValidation UTCTime)

-- | Helper function for validation
--
-- @since 0.1.0.0
validate ::
  -- | predicate
  (a -> Bool) ->
  -- | error to throw if validation fail
  e ->
  -- | input to check
  a ->
  V.Validation (NonEmpty e) a
validate p err raw = if p raw then V.Success raw else V.failure err

-- | Convenient type synonym
--
-- @since 0.1.0.0
type WithValidation = V.Validation ValidationErr

-- | An newtype with no validation
--
-- Useful for deriving instances like ('FromJSON', 'FromHttpApiData', etc) for type with no validation using __DerivingVia__
--
-- __Usage__:
--
-- > deriving via (WithNoValidation a) instance FromJSON (WithValidation a)
-- > deriving via (WithValidation (NoValidation a)) instance FromJSON (WithValidation a-like)
--
-- @since 0.1.0.0
newtype NoValidation a = NoValidation a deriving (Generic, FromHttpApiData, FromJSON)

-- | Convenient type synonym
--
-- __Usage__:
--
-- > deriving via (WithNoValidation a) instance FromJSON (WithValidation a-like)
--
-- @since 0.1.0.0
type WithNoValidation a = WithValidation (NoValidation a)

-- | No Validation when parse from json
--
-- @since 0.1.0.0
instance FromJSON a => FromJSON (WithNoValidation a) where
  parseJSON = pure <<$>> parseJSON

-- | No Validation when parse from HttpApiData
--
-- @since 0.1.0.0
instance FromHttpApiData a => FromHttpApiData (WithNoValidation a) where
  parseQueryParam = pure <<$>> parseQueryParam
