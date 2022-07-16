{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Field.Slug.Ord where

import Data.Field.Slug (Slug (..))

deriving instance Ord Slug
