{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Description : Field
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Field for Description
--
-- @since 0.2.0.0
module Field.Description where

import Data.Aeson (FromJSON, ToJSON)
import Util.Validation (NoValidation (..), WithNoValidation, WithValidation)

-- | @since 0.2.0.0
newtype Description = Description Text deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Description)
