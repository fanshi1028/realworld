{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Field
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Field for Time
--
-- @since 0.4.0.0
module Data.Field.Time where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Time (UTCTime)
import qualified Data.Time as T (getCurrentTime)
import Data.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)

-- | @since 0.3.0.0
newtype Time = Time UTCTime deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

-- | @since 0.3.0.0
deriving via (WithNoValidation Time) instance FromJSON (WithValidation Time)

-- | @since 0.3.0.0
getCurrentTime :: IO Time
getCurrentTime = Time <$> T.getCurrentTime
