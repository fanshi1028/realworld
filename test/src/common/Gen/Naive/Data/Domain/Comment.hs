{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Domain.Comment where

import Data.Domain.Comment (CommentWithAuthorProfile (..))
import Gen.Naive.Data.Domain.User ()
import Gen.Naive.Data.Field.Time ()
import Gen.Naive.Data.Storage.Map.HasStorage ()
import Test.QuickCheck (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)

instance Arbitrary CommentWithAuthorProfile where
  arbitrary = genericArbitrary
  shrink = genericShrink
