{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Domain.User where

import Data.Domain.User (UserAuthWithToken (..), UserProfile (..))
import Gen.Naive.Data.Authentication.HasAuth ()
import Gen.Naive.Data.Authentication.HasToken ()
import Test.QuickCheck (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)

instance Arbitrary UserProfile where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary UserAuthWithToken where
  arbitrary = genericArbitrary
  shrink = genericShrink
