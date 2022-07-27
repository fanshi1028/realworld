{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Ord.Data.Field.Username where

import Data.Field.Username (Username (..))

deriving instance Ord Username
