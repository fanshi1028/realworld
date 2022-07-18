{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Username where

import Data.Field.Username (Username (..))
import qualified Faker.FunnyName as Funny
import qualified Faker.Name as Name
import Gen.Realistic.Util (Realistic (..), quickOneOf)
import Test.QuickCheck (Arbitrary (arbitrary))

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(Realistic Username)
-- ["Ben Lyon","Anna Conda","Idell Lindgren","Pat Shields Feil","Gov. Shad Reichel Kuhn","Edith Romaguera Ortiz","Ashlee Altenwerth Reynolds","Ben Lyon","Sixta Langworth","Mariana Mueller Spinka","Hope Mayert White"]
instance Arbitrary (Realistic Username) where
  arbitrary =
    Realistic . Username
      <$> quickOneOf
        [ Name.nameWithMiddle,
          Funny.name,
          Name.name
        ]
