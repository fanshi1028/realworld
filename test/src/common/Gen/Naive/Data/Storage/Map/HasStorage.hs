{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Storage.Map.HasStorage where

import Data.Domain (Domain (..))
import Data.Storage.Map (ContentOf)
import Gen.Naive.Data.Field.Body ()
import Gen.Naive.Data.Field.Description ()
import Gen.Naive.Data.Field.Time ()
import Gen.Naive.Data.Field.Title ()
import Gen.Naive.Data.Storage.Map.HasId ()
import Test.QuickCheck (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)

instance Arbitrary (ContentOf 'Article) where
  arbitrary = genericArbitrary
  shrink = genericShrink
