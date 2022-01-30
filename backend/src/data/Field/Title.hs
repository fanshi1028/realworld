{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Description : Field
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Field for Title
--
-- @since 0.2.0.0
module Field.Title where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withText)
import qualified Data.Text as T
import Util.Validation (WithValidation, validate)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.2.0.0
newtype Title = Title Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.2.0.0
instance FromJSON (WithValidation Title) where
  parseJSON = withText "title" $ pure . (Title <<$>> validate (not . T.null) "null title")
-- ^
-- ==== Success
-- >>> eitherDecode' @(WithValidation Title) "\"jfowjfw@mmm\""
-- Right (Success "jfowjfw@mmm")
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation Title) "\"\""
-- Right (Failure ("null title" :| []))
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation Title) "{}"
-- Left "Error in $: parsing title failed, expected String, but encountered Object"
