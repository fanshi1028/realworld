{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Domain.Article where

import Data.Domain.Article (ArticleWithAuthorProfile (..))
import Gen.Naive.Data.Domain.User ()
import Gen.Naive.Data.Field.Tag ()
import Gen.Naive.Data.Storage.Map.HasStorage ()
import Test.QuickCheck (Arbitrary (arbitrary, shrink), genericShrink)
import Test.QuickCheck.Arbitrary.ADT (genericArbitrary)

instance Arbitrary ArticleWithAuthorProfile where
  arbitrary = genericArbitrary
  shrink = genericShrink
