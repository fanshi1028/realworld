{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @since 0.2.0.0
module Field.Title where

import Data.Aeson (FromJSON (parseJSON), ToJSON, withText)
import qualified Data.Text as T
import Util.Validation (WithValidation, validate)

-- | @since 0.2.0.0
newtype Title = Title Text
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | @since 0.2.0.0
instance FromJSON (WithValidation Title) where
  parseJSON = withText "title" $ pure . (Title <<$>> validate (not . T.null) "null title")
