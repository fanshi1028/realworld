{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @since 0.2.0.0
module Field.Email (Email (Email)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withText)
import qualified Data.Text as T
import Util.Validation (WithValidation, validate)

-- | @since 0.2.0.0
newtype Email = Email Text deriving newtype (Show, Eq, Hashable, ToJSON, FromJSON)

-- | __FIXME__: Refine validation of 'Email'
--
-- @since 0.2.0.0
instance FromJSON (WithValidation Email) where
  parseJSON = withText "email" $ pure . (Email <<$>> validate (not . T.null) "null email")
