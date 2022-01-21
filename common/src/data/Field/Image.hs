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
-- Field for Image
--
-- @since 0.4.0.0
module Data.Field.Image where

import Data.Aeson (FromJSON, ToJSON)
import Data.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)

-- | @since 0.2.0.0
newtype Image
  = -- | the link of the image
    Image Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Image)
