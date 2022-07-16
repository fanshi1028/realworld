{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Field.Username.Ord where

import Data.Field.Username (Username (..))

deriving instance Ord Username
