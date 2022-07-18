{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Storage.Map.HasCreate where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasCreate (CreateOf (..), HasCreate (..))
import Gen.Realistic.Data.Field.Body ()
import Gen.Realistic.Data.Field.Description ()
import Gen.Realistic.Data.Field.Email ()
import Gen.Realistic.Data.Field.Password ()
import Gen.Realistic.Data.Field.Tag ()
import Gen.Realistic.Data.Field.Title ()
import Gen.Realistic.Data.Field.Username ()
import Gen.Realistic.Util (Realistic (..), arbitraryRealistic, genRealisticTitle, shrinkRealistic)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Instances.Text ()

instance Arbitrary (Realistic (CreateOf 'User)) where
  arbitrary = Realistic <$> (UserCreate <$> arbitraryRealistic <*> arbitraryRealistic <*> arbitraryRealistic)
  shrink (Realistic (UserCreate u em pw)) =
    Realistic <$> (UserCreate <$> shrinkRealistic u <*> shrinkRealistic em <*> shrinkRealistic pw)

instance Arbitrary (Realistic (CreateOf 'Article)) where
  arbitrary = Realistic <$> (ArticleCreate <$> arbitraryRealistic <*> arbitraryRealistic <*> arbitraryRealistic <*> arbitraryRealistic)
  shrink (Realistic (ArticleCreate tt d b ts)) =
    Realistic <$> (ArticleCreate <$> shrinkRealistic tt <*> shrinkRealistic d <*> shrinkRealistic b <*> shrinkRealistic ts)

instance Arbitrary (Realistic (CreateOf 'Comment)) where
  arbitrary = Realistic . CommentCreate <$> genRealisticTitle
  shrink (Realistic (CommentCreate c)) = Realistic . CommentCreate <$> shrink c
