{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Time where

import Data.Field.Time (Time (..))
import Gen.Naive.Data.Field.Time ()
import Gen.Realistic.Util (Realistic (..))
import Test.QuickCheck (Arbitrary)

deriving newtype instance Arbitrary (Realistic Time)
