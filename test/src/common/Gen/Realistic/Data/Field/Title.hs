{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Title where

import Data.Field.Title (Title (..))
import Gen.Realistic.Util (Realistic (..), genRealisticTitle)
import Test.QuickCheck (Arbitrary (arbitrary))

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic Title)
-- ["Baptism of Fire","Use of Weapons","Beyond the Mexique Bay","The Hydrogen Sonata","Naib","Padishah Emperor","Judge of the Change","Count","Baptism of Fire","No Longer at Ease","Harry Potter and the Goblet of Fire"]
instance Arbitrary (Realistic Title) where
  arbitrary = Realistic . Title <$> genRealisticTitle
