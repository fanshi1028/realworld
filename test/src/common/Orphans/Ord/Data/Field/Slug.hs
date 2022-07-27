{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Ord.Data.Field.Slug where

import Data.Field.Slug (Slug (..))

deriving instance Ord Slug
