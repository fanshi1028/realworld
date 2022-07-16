{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Field.Tag.Ord where

import Data.Field.Tag (Tag (..))

deriving instance Ord Tag
