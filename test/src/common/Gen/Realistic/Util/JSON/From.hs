{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Util.JSON.From where

import Data.Util.JSON.From (In (..))
import Gen.Realistic.Util (Realistic (Realistic))
import Test.QuickCheck (Arbitrary (arbitrary))

instance Arbitrary (Realistic a) => Arbitrary (Realistic (In a)) where
  arbitrary = arbitrary >>= \(Realistic a) -> pure (Realistic (In a))
