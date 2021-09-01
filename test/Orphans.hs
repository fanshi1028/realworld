{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
module Orphans where

import Domain.User (UserR (..))
import Domain.Util.Field (Bio (Bio), Email (Email), Image (Image), Username (Username))
import Domain.Util.JSON.To (Out)
import Faker.Combinators (oneof)
import qualified Faker.Company as Company
import qualified Faker.FunnyName as Funny
import qualified Faker.Name as Name
import qualified Faker.TvShow.SiliconValley as SiliconValley
import qualified Faker.TvShow.TheItCrowd as TheItCrowd
import Test.QuickCheck (Arbitrary (arbitrary), sample, sample')
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary, genericArbitrary)
import Test.QuickCheck.Gen.Faker (fakeQuickcheck)
import Test.QuickCheck.Arbitrary (arbitraryASCIIChar)

qucikOneOf = fakeQuickcheck . oneof

-- * Email

-- >>> sample' (arbitrary @Email)
-- ["bertram@piedpiper.test","Denyse_OConner-733897692@Marvin_LLC.net","russ@threecommaclub.test","victoria.reynholm@reynholm.test","Rep_Britany_Schultz-380839071@Hayes_LLC.co","Herma_Hilll-564648444@Bartell_Inc.net","russ@threecommaclub.test","dinesh@piedpiper.test","bertram@piedpiper.test","daniel.carey@reynholm.test","laurie@raviga.test"]
instance Arbitrary Email where
  arbitrary = Email <$> qucikOneOf [Company.email, SiliconValley.email, TheItCrowd.emails]

-- >>> sample' (arbitrary @Username)
-- ["Brooke Trout","Estell Howell","Brook Lynn Bridge","Theron Schaefer","Barry Cade","Twana Smith","Onie Bartoletti Barton","Lonna Roob","Herta Ritchie O'Reilly","Lou Dan Obseen","Jone Ratke Collier"]
instance Arbitrary Username where
  arbitrary = Username <$> qucikOneOf [Name.nameWithMiddle, Funny.name, Name.name]

instance Arbitrary Bio where
  arbitrary = Bio <$> qucikOneOf [pure " wefwe"]

instance Arbitrary Image where
  arbitrary = Image <$> qucikOneOf [pure " ewfew"]

instance Arbitrary (UserR "auth") where
  arbitrary = genericArbitrary

-- NOTE: validity approach?
-- FIXME
-- >>> sample' (arbitrary @(UserR "token"))
-- ["","8\995314","<kwE","","4}","Y\f\GS\8264o\141689TD","\1092918\ETX\tN\SYN0\34037","6Xc\SOvk\EM","(N\SI\1064492t3J#5o\60377o\DC3t\a5","","55l"]
deriving instance Arbitrary (UserR "token")

-- NOTE: In could gen invalid instance which Out gen only valid instance>

instance Arbitrary (UserR "authWithToken") where
  arbitrary = UserAuthWithToken <$> arbitrary <*> arbitrary

instance ToADTArbitrary (Out (UserR "authWithToken"))
