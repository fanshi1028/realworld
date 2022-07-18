{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Tag where

import Data.Field.Tag (Tag (..))
import Gen.Realistic.Util (Realistic (..), genRealisticBuzzword)
import Test.QuickCheck (Arbitrary (arbitrary))

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(Realistic Tag)
-- ["Cloned","stygian","expansion play","mission-critical","fainted","tofu","crucifix","get a pulse on","penetrate the market","Function-based","small batch"]
instance Arbitrary (Realistic Tag) where
  arbitrary = Realistic . Tag <$> genRealisticBuzzword
