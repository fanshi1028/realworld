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
-- Field for Slug
--
-- @since 0.4.0.0
module Data.Field.Slug where

import Data.Aeson (FromJSON, ToJSON)
import Data.Field.Title (Title (Title))
import qualified Data.Text as T (intercalate, toLower)
import Data.Util.Validation (NoValidation (..), WithNoValidation, WithValidation)
import Servant (FromHttpApiData)

-- | @since 0.2.0.0
newtype Slug = Slug Text deriving newtype (Show, Eq, ToJSON, Hashable)

-- | @since 0.2.0.0
-- __FIXME__: Slug need validation?
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Slug)

-- | @since 0.2.0.0
-- __FIXME__: Slug need validation?
deriving via (WithNoValidation Text) instance FromHttpApiData (WithValidation Slug)

-- | @since 0.2.0.0
titleToSlug :: Title -> Slug
titleToSlug (Title t) = Slug $ T.intercalate "-" $ words $ T.toLower t
