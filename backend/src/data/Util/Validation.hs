{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- |
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

    -- * Helpers
    validate,
  )
where

import Data.Aeson (FromJSON (parseJSON), withArray)
import Data.Aeson.Types (withText)
import qualified Data.HashSet as HS (fromList)
import Data.Password.Argon2 (Password, mkPassword)
import Data.Password.Validate (ValidationResult (InvalidPassword, ValidPassword), defaultPasswordPolicy_, validatePassword)
import qualified Data.Semigroup as SG (Last (Last))
import Data.Time (UTCTime)
import Servant (FromHttpApiData (parseQueryParam))
import Util.Error (ValidationErr)
import qualified Validation as V (Validation (Failure, Success), failure)

-- | Validate password with default password policy
--
-- @since 0.2.0.0
instance FromJSON (WithValidation Password) where
  parseJSON =
    withText "password" $ \(mkPassword -> pw) -> pure $
      case validatePassword defaultPasswordPolicy_ pw of
        ValidPassword -> V.Success pw
        InvalidPassword irs ->
          maybe (error "Impossible: password must be invalidated with a reason!") V.Failure $
            nonEmpty $ show @Text <$> irs

-- | @since 0.1.0.0
instance FromJSON (WithValidation a) => FromJSON (WithValidation [a]) where
  parseJSON = withArray "array" $ mapM parseJSON >=> pure . sequenceA . toList

-- | @since 0.1.0.0
instance (Eq r, Hashable r, FromJSON (WithValidation r)) => FromJSON (WithValidation (HashSet r)) where
  parseJSON = fmap HS.fromList . sequenceA <<$>> parseJSON

-- | @since 0.1.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Text)

-- | @since 0.1.0.0
deriving via (WithNoValidation UTCTime) instance FromJSON (WithValidation UTCTime)

-- | For parsing partial update patch with 'HKD'
--
-- @since 0.2.0.0
instance (FromJSON a, FromJSON (WithValidation a)) => FromJSON (WithValidation (Maybe (SG.Last a))) where
  parseJSON = (Just . SG.Last <$>) <<$>> parseJSON @(WithValidation a)

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
