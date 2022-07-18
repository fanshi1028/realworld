{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Util.JSON.To where

import Data.Util.JSON.To (Out (..))
import Test.QuickCheck (Arbitrary)

deriving newtype instance Arbitrary a => Arbitrary (Out a)
