{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | @since 0.2.0.0
module Field.Slug where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T (intercalate, toLower)
import Field.Title (Title (Title))
import Servant (FromHttpApiData)
import Util.Validation (NoValidation (..), WithNoValidation, WithValidation)

-- | @since 0.2.0.0
newtype Slug = Slug Text deriving newtype (Show, Eq, ToJSON, Hashable)

-- | __FIXME__: Slug need validation?
--
--  @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromJSON (WithValidation Slug)

-- | __FIXME__: Slug need validation?
--
--  @since 0.2.0.0
deriving via (WithNoValidation Text) instance FromHttpApiData (WithValidation Slug)

-- | @since 0.2.0.0
titleToSlug :: Title -> Slug
titleToSlug (Title t) = Slug $ T.intercalate "-" $ words $ T.toLower t
