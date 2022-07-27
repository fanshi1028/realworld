{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Ord.Data.Field.Tag where

import Data.Field.Tag (Tag (..))

deriving instance Ord Tag
