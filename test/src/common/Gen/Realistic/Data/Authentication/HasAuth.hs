{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Authentication.HasAuth where

import Data.Authentication.HasAuth (AuthOf (UserAuth), LoginOf (UserLogin))
import Data.Domain (Domain (..))
import Gen.Realistic.Data.Field.Bio ()
import Gen.Realistic.Data.Field.Email ()
import Gen.Realistic.Data.Field.Image ()
import Gen.Realistic.Data.Field.Password ()
import Gen.Realistic.Data.Field.Username ()
import Gen.Realistic.Util (Realistic (Realistic), arbitraryRealistic, shrinkRealistic)
import Test.QuickCheck (Arbitrary (arbitrary, shrink))

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(Realistic (AuthOf 'User))
-- [UserAuth {email = "erlich@bachmanity.test", username = "Ben Lyon", bio = "For ever and a day.", image = "imageTBE"},UserAuth {email = "laurie@raviga.test", username = "Arie Aufderhar Wisozk", bio = " audiences are going to adore your tour de force performance as the forceful denim-clad court reporter in 'The Court Reporter Sported Jorts', the jet-setting jort-sporting court reporter story", image = "imageTBE"},UserAuth {email = "Merry_Okuneva-541711812@Williamson_LLC.net", username = "Hubert Zieme Luettgen", bio = "When you play a game of thrones you win or you die.", image = "imageTBE"},UserAuth {email = "daniel.carey@reynholm.test", username = "Doug Updegrave", bio = "For ever and a day.", image = "imageTBE"},UserAuth {email = "jared@piedpiper.test", username = "Mr. Lorenzo Schaefer Cartwright", bio = "I'm the dude, so that's what you call me. That or, uh His Dudeness, or uh Duder, or El Duderino, if you're not into the whole brevity thing.", image = "imageTBE"},UserAuth {email = "Tommie_Huel-104543348@Franecki_Inc.net", username = "Mable Kerluke", bio = "There is nothing either good or bad, but thinking makes it so.", image = "imageTBE"},UserAuth {email = "Greta_Rice-871679444@Zemlak_LLC.net", username = "Frida Olson Kerluke", bio = "The world is grown so bad that wrens make prey where eagles dare not perch.", image = "imageTBE"},UserAuth {email = "johan@hotmail.test", username = "Janeen Reinger", bio = "Portnoy finds joy in hoi polloi boy toy", image = "imageTBE"},UserAuth {email = "Augustus_Smitham-392056915@Dare_Group.name", username = "Mr. Libby Quitzon Donnelly", bio = "When you play a game of thrones you win or you die.", image = "imageTBE"},UserAuth {email = "erlich@bachmanity.test", username = "Jimmy DeLocke", bio = "Portnoy finds joy in hoi polloi boy toy", image = "imageTBE"},UserAuth {email = "johan@hotmail.test", username = "Mr. Brigid Schaden Champlin", bio = "For ever and a day.", image = "imageTBE"}]
instance Arbitrary (Realistic (AuthOf 'User)) where
  arbitrary = Realistic <$> (UserAuth <$> arbitraryRealistic <*> arbitraryRealistic <*> arbitraryRealistic <*> arbitraryRealistic)
  shrink (Realistic (UserAuth em user bio' im)) =
    Realistic <$> (UserAuth <$> shrinkRealistic em <*> shrinkRealistic user <*> shrinkRealistic bio' <*> shrinkRealistic im)

instance Arbitrary (Realistic (LoginOf 'User)) where
  arbitrary = Realistic <$> (UserLogin <$> arbitraryRealistic <*> arbitraryRealistic)
  shrink (Realistic (UserLogin em pw)) = Realistic <$> (UserLogin <$> shrinkRealistic em <*> shrinkRealistic pw)
