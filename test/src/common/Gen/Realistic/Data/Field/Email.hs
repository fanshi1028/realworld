{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Email where

import Data.Field.Email (Email (..))
import qualified Faker.Company as Company
import qualified Faker.TvShow.SiliconValley as SiliconValley
import qualified Faker.TvShow.TheItCrowd as TheItCrowd
import Gen.Realistic.Util (Realistic (..), quickOneOf)
import Test.QuickCheck (Arbitrary (arbitrary))

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(Realistic Email)
-- ["Mr_Claud_Donnelly-904574528@Collins_Group.com","Mr_Juli_McKenzie-777488160@Moore_Inc.com","johan@hotmail.test","Darcy_Tromp-251839356@Schoen_Inc.net","paul@reynholm.test","russ@threecommaclub.test","bertram@piedpiper.test","Michel_Rowe-522760967@Schoen_LLC.net","Mr_Jay_Brakus-535971968@Goodwin_and_Sons.com","Buford_Bailey-913725363@Walker_Group.name","daniel.carey@reynholm.test"]
instance Arbitrary (Realistic Email) where
  arbitrary = Realistic . Email <$> quickOneOf [Company.email, SiliconValley.email, TheItCrowd.emails]
