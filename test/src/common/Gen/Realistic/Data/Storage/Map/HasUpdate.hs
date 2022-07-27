{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Storage.Map.HasUpdate where

import Data.Domain (Domain (Article, User))
import Data.Generic.HKD (build, construct)
import Data.Storage.Map.HasUpdate (Patch, UpdateOf)
import Gen.Realistic.Data.Field.Bio ()
import Gen.Realistic.Data.Field.Body ()
import Gen.Realistic.Data.Field.Description ()
import Gen.Realistic.Data.Field.Email ()
import Gen.Realistic.Data.Field.Image ()
import Gen.Realistic.Data.Field.Password ()
import Gen.Realistic.Data.Field.Title ()
import Gen.Realistic.Data.Field.Username ()
import Gen.Realistic.Util (Realistic (..), arbitraryRealistic)
import Test.QuickCheck (Arbitrary (arbitrary))

instance Arbitrary (Realistic (Patch (UpdateOf 'User))) where
  arbitrary =
    Realistic
      <$> construct
        ( build @(Patch (UpdateOf 'User))
            arbitraryRealistic
            arbitraryRealistic
            arbitraryRealistic
            arbitraryRealistic
            arbitraryRealistic
        )

instance Arbitrary (Realistic (Patch (UpdateOf 'Article))) where
  arbitrary =
    Realistic
      <$> construct
        ( build @(Patch (UpdateOf 'Article))
            arbitraryRealistic
            arbitraryRealistic
            arbitraryRealistic
        )
