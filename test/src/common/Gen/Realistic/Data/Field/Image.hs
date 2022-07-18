{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Image where

import Data.Field.Image (Image (..))
import Gen.Realistic.Util (Realistic (..), quickOneOf)
import Test.QuickCheck (Arbitrary (arbitrary))

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(Realistic Image)
-- ["imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE"]
instance Arbitrary (Realistic Image) where
  arbitrary = Realistic . Image <$> quickOneOf [pure "imageTBE"]
