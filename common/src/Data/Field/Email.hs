{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Description : Field
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Field for Email
--
-- @since 0.4.0.0
module Data.Field.Email (Email (Email)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withText)
import qualified Data.Text as T
import Data.Util.Validation (WithValidation, validate)

-- $setup
-- >>> import Data.Aeson (eitherDecode')

-- | @since 0.2.0.0
newtype Email = Email Text deriving newtype (Show, Eq, Hashable, ToJSON, FromJSON)

-- | @since 0.2.0.0
-- __FIXME__: Refine validation of 'Email'
instance FromJSON (WithValidation Email) where
  parseJSON = withText "email" $ pure . (Email <<$>> validate (not . T.null) "null email")
-- ^
-- ==== Success
-- >>> eitherDecode' @(WithValidation Email) "\"jfowjfw@mmm\""
-- Right (Success "jfowjfw@mmm")
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation Email) "\"\""
-- Right (Failure ("null email" :| []))
--
-- ==== Validation Fail
-- >>> eitherDecode' @(WithValidation Email) "{}"
-- Left "Error in $: parsing email failed, expected String, but encountered Object"
