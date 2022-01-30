{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Helper
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- When we get data from the outside world, we do validation.
-- So we integrate validation with instance like 'FromJSON', 'FromHttpApiData', etc
--
-- @since 0.1.0.0
module Util.Validation
  ( -- * Types
    WithValidation,
    NoValidation (..),
    WithNoValidation,

    -- * Error
    ValidationErr,

    -- * Helpers
    validate,
  )
where

import Data.Aeson (FromJSON (parseJSON), withArray)
import qualified Data.Semigroup as SG (Last (Last))
import Servant (FromHttpApiData (parseQueryParam))
import qualified Validation as V (Validation (Success), failure)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.1.0.0
instance FromJSON (WithValidation a) => FromJSON (WithValidation [a]) where
  parseJSON = withArray "array" $ mapM parseJSON >=> pure . sequenceA . toList

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Text)

-- | @since 0.2.0.0
-- For parsing partial update patch with 'HKD'
instance (FromJSON a, FromJSON (WithValidation a)) => FromJSON (WithValidation (Maybe (SG.Last a))) where
  parseJSON = (Just . SG.Last <$>) <<$>> parseJSON @(WithValidation a)

-- | @since 0.1.0.0
-- Helper function for validation
validate ::
  -- | predicate
  (a -> Bool) ->
  -- | error to throw if validation fail
  e ->
  -- | input to check
  a ->
  V.Validation (NonEmpty e) a
validate p err raw = if p raw then V.Success raw else V.failure err

-- | @since 0.1.0.0
-- Convenient type synonym
type WithValidation = V.Validation ValidationErr

-- | @since 0.1.0.0
-- An newtype with no validation
--
-- Useful for deriving instances like ('FromJSON', 'FromHttpApiData', etc) for type with no validation using __DerivingVia__
--
-- __Usage__:
--
-- > deriving via (WithNoValidation a) instance FromJSON (WithValidation a)
-- > deriving via (WithValidation (NoValidation a)) instance FromJSON (WithValidation a-like)
newtype NoValidation a = NoValidation a deriving (Generic, FromHttpApiData, FromJSON)

-- | @since 0.1.0.0
-- Convenient type synonym
--
-- __Usage__:
--
-- > deriving via (WithNoValidation a) instance FromJSON (WithValidation a-like)
--
-- remember to import the constructor of 'NoValidation', when deriving via 'WithNoValidation'
type WithNoValidation a = WithValidation (NoValidation a)

-- | @since 0.1.0.0
-- No Validation when parse from json
instance FromJSON a => FromJSON (WithNoValidation a) where
  parseJSON = pure <<$>> parseJSON

-- | @since 0.1.0.0
-- No Validation when parse from HttpApiData
instance FromHttpApiData a => FromHttpApiData (WithNoValidation a) where
  parseQueryParam = pure <<$>> parseQueryParam

-- | @since 0.2.0.0
-- Just use 'Text' to represent all validation errors. When it happens, there will be a nonempty list of them.
type ValidationErr = NonEmpty Text

-- newtype ValidationErr = ValidationErr (NonEmpty Text)
