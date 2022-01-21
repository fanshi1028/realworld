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
-- Field for Username
--
-- @since 0.4.0.0
module Data.Field.Username where

import Data.Aeson (FromJSON, ToJSON)
import Data.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import Servant (FromHttpApiData)

-- | @since 0.2.0.0
newtype Username = Username Text
  deriving newtype (Show, Eq, Hashable, ToJSON, FromJSON)

-- | @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Username)

-- | @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromHttpApiData (WithValidation Username)
