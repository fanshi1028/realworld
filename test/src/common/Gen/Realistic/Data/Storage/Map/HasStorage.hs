{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Storage.Map.HasStorage where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasStorage (IdOf (..))
import Gen.Realistic.Data.Field.Username ()
import Gen.Realistic.Util (Realistic (..), arbitraryRealistic)
import Test.QuickCheck (Arbitrary (arbitrary))

instance Arbitrary (Realistic (IdOf 'User)) where
  arbitrary = Realistic . UserId <$> arbitraryRealistic
