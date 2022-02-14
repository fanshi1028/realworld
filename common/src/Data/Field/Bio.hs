{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Description : Field
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Field for Bio
--
-- @since 0.4.0.0
module Data.Field.Bio where

import Data.Aeson (FromJSON, ToJSON)
import Data.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)

-- | @since 0.2.0.0
newtype Bio = Bio Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Bio)
