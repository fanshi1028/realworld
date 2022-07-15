{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | @since 0.2.0.0
module Gen.Realistic where

import Data.Authentication.HasAuth (AuthOf (..), HasAuth (..), LoginOf (UserLogin))
import Data.Domain (Domain (Article, Comment, User))
import Data.Field.Bio (Bio (Bio))
import Data.Field.Body (Body (Body))
import Data.Field.Description (Description (Description))
import Data.Field.Email (Email (Email))
import Data.Field.Image (Image (Image))
import Data.Field.Password (Password (Password))
import Data.Field.Tag (Tag (Tag))
import Data.Field.Time (Time)
import Data.Field.Title (Title (Title))
import Data.Field.Username (Username (Username))
import Data.Generic.HKD (build, construct)
import Data.Password.Argon2 (mkPassword)
import Data.Password.Validate (ValidationResult (ValidPassword), defaultPasswordPolicy_, validatePassword)
import qualified Data.Semigroup as SG
import Data.Storage.Map (IdOf (UserId), Patch, UpdateOf)
import Data.Storage.Map.HasCreate (CreateOf (..))
import Data.Util.JSON.From (In (In))
import Faker (Fake)
import qualified Faker.Book as Book
import qualified Faker.Book.CultureSeries as CultureSeries
import qualified Faker.Book.Dune as Dune
import qualified Faker.Book.Lovecraft as Lovecraft
import qualified Faker.Cannabis as Cannabis
import Faker.Combinators (oneof)
import qualified Faker.Company as Company
import qualified Faker.FunnyName as Funny
import qualified Faker.Game.Witcher as Witcher
import qualified Faker.GreekPhilosophers as GreekPhilosophers
import qualified Faker.Hipster as Hipster
import qualified Faker.JapaneseMedia.OnePiece as OnePiece
import qualified Faker.Marketing as Marketing
import qualified Faker.Movie.HarryPotter as HarryPotter
import qualified Faker.Movie.Lebowski as Lebowski
import qualified Faker.Movie.VForVendetta as VForVendetta
import qualified Faker.Name as Name
import qualified Faker.Quote.Shakespeare as Shakespeare
import qualified Faker.TvShow.BoJackHorseman as BojackHorseman
import qualified Faker.TvShow.GameOfThrones as GameOfThrones
import qualified Faker.TvShow.SiliconValley as SiliconValley
import qualified Faker.TvShow.SouthPark as SouthPark
import qualified Faker.TvShow.TheItCrowd as TheItCrowd
import Gen.Naive ()
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, arbitraryASCIIChar, elements, frequency, listOf, suchThat)
import Test.QuickCheck.Gen.Faker (fakeQuickcheck)

-- $setup
-- >>> import Test.QuickCheck (sample')

-- | @since 0.2.0.0
newtype Realistic a = Realistic {getRealistic :: a} deriving newtype (Show)

arbitraryRealistic :: Arbitrary (Realistic a) => Gen a
arbitraryRealistic = getRealistic <$> arbitrary

shrinkRealistic :: Arbitrary (Realistic a) => a -> [a]
shrinkRealistic = getRealistic <<$>> shrink . Realistic

-- | @since 0.2.0.0
--
-- Generate value from one of the Faker generator
quickOneOf :: [Fake a] -> Gen a
quickOneOf = fakeQuickcheck . oneof

-- | @since 0.2.0.0
genRealisticText :: Gen Text
genRealisticText =
  quickOneOf
    [ OnePiece.quotes,
      Lebowski.quotes,
      BojackHorseman.quote,
      GameOfThrones.quotes,
      SouthPark.quotes,
      Shakespeare.hamlet,
      Shakespeare.asYouLikeIt,
      Shakespeare.kingRichardIii,
      Shakespeare.romeoAndJuliet,
      VForVendetta.speeches,
      VForVendetta.quotes,
      BojackHorseman.tongueTwister,
      GreekPhilosophers.quotes
    ]

-- | @since 0.2.0.
genRealisticTitle :: Gen Text
genRealisticTitle =
  quickOneOf
    [ Book.title,
      Dune.titles,
      CultureSeries.books,
      HarryPotter.books,
      Witcher.books
    ]

-- | @since 0.2.0.0
genRealisticBuzzword :: Gen Text
genRealisticBuzzword = quickOneOf [Company.buzzword, Marketing.buzzwords, Cannabis.buzzwords, Hipster.words, Lovecraft.words]

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic Email)
-- ["Mr_Claud_Donnelly-904574528@Collins_Group.com","Mr_Juli_McKenzie-777488160@Moore_Inc.com","johan@hotmail.test","Darcy_Tromp-251839356@Schoen_Inc.net","paul@reynholm.test","russ@threecommaclub.test","bertram@piedpiper.test","Michel_Rowe-522760967@Schoen_LLC.net","Mr_Jay_Brakus-535971968@Goodwin_and_Sons.com","Buford_Bailey-913725363@Walker_Group.name","daniel.carey@reynholm.test"]
instance Arbitrary (Realistic Email) where
  arbitrary = Realistic . Email <$> quickOneOf [Company.email, SiliconValley.email, TheItCrowd.emails]

-- | @since 0.2.0.0
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

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic Bio)
-- ["See how she leans her cheek upon her hand. Oh, that I were a glove upon that hand that I might touch that cheek!","When you play a game of thrones you win or you die.","The world is grown so bad that wrens make prey where eagles dare not perch.","The world is grown so bad that wrens make prey where eagles dare not perch.","You know what they say: You can't teach a gay dog straight tricks","The world is grown so bad that wrens make prey where eagles dare not perch.","Philosophy is the highest music.","Dead on the inside, dead on the outside","There is nothing either good or bad, but thinking makes it so.","I'm the dude, so that's what you call me. That or, uh His Dudeness, or uh Duder, or El Duderino, if you're not into the whole brevity thing.","Laughter is poison to fear."]
instance Arbitrary (Realistic Bio) where
  arbitrary = Realistic . Bio <$> genRealisticText

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic Image)
-- ["imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE","imageTBE"]
instance Arbitrary (Realistic Image) where
  arbitrary = Realistic . Image <$> quickOneOf [pure "imageTBE"]

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic Title)
-- ["Baptism of Fire","Use of Weapons","Beyond the Mexique Bay","The Hydrogen Sonata","Naib","Padishah Emperor","Judge of the Change","Count","Baptism of Fire","No Longer at Ease","Harry Potter and the Goblet of Fire"]
instance Arbitrary (Realistic Title) where
  arbitrary = Realistic . Title <$> genRealisticTitle

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic Description)
-- ["When you play a game of thrones you win or you die.","The world is grown so bad that wrens make prey where eagles dare not perch.","Portnoy finds joy in hoi polloi boy toy","Dad, Tom Cruise won't come out of the closet!","Portnoy finds joy in hoi polloi boy toy","When you play a game of thrones you win or you die.","Life is short butters, & thats why you have to do whatever you want all the time","I know there's no way I can convince you this is not one of their tricks, but I don't care, I am me. My name is Valerie, I don't think I'll live much longer and I wanted to tell someone about my life. This is the only autobiography I'll ever write, and god, I'm writing it on toilet paper. I was born in Nottingham in 1985, I don't remember much of those early years, but I do remember the rain. My grandmother owned a farm in Tuttlebrook, and she use to tell me that god was in the rain. I passed my 11th lesson into girl's grammar; it was at school that I met my first girlfriend, her name was Sara. It was her wrists. They were beautiful. I thought we would love each other forever. I remember our teacher telling us that is was an adolescent phase people outgrew. Sara did, I didn't. In 2002, I fell in love with a girl named Christina. That year I came out to my parents. I couldn't have done it without Chris holding my hand. My father wouldn't look at me, he told me to go and never come back. My mother said nothing. But I had only told them the truth, was that so selfish? Our integrity sells for so little, but it is all we really have. It is the very last inch of us, but within that inch, we are free. I'd always known what I wanted to do with my life, and in 2015 I starred in my first film, 'The Salt Flats'. It was the most important role of my life, not because of my career, but because that was how I met Ruth. The first time we kissed, I knew I never wanted to kiss any other lips but hers again. We moved to a small flat in London together. She grew Scarlet Carsons for me in our window box, and our place always smelled of roses. Those were there best years of my life. But America's war grew worse, and worse. And eventually came to London. After that there were no roses anymore. Not for anyone. I remember how the meaning of words began to change. How unfamiliar words like 'collateral' and 'rendition' became frightening. While things like Norse Fire and The Articles of Allegiance became powerful, I remember how different became dangerous. I still don't understand it, why they hate us so much. They took Ruth while she was out buying food. I've never cried so hard in my life. It wasn't long till they came for me. It seems strange that my life should end in such a terrible place, but for three years, I had roses, and apologized to no one. I shall die here. Every inch of me shall perish. Every inch, but one. An Inch, it is small and it is fragile, but it is the only thing the world worth having. We must never lose it or give it away. We must never let them take it from us. I hope that whoever you are, you escape this place. I hope that the world turns and that things get better. But what I hope most of all is that you understand what I mean when I tell you that even though I do not know you, and even though I may never meet you, laugh with you, cry with you, or kiss you. I love you. With all my heart, I love you. -Valerie","The secret to humor is surprise.","Dead on the inside, dead on the outside","Oh, not tonight Bishop... not tonight!"]
instance Arbitrary (Realistic Description) where
  arbitrary = Realistic . Description <$> genRealisticText

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic Body)
-- ["A lion doesn't concern itself with the opinion of sheep.","A few vices are sufficient to darken many virtues.","See how she leans her cheek upon her hand. Oh, that I were a glove upon that hand that I might touch that cheek!","I wish I wasn't afraid all the time, but... I am.","Stop counting only those things you have lost! What is gone, is gone! So ask yourself this. What is there... that still remains to you?!","Hey, I know that guy. He's a nihilist. Karl Hungus.","Dead on the inside, dead on the outside","Dead on the inside, dead on the outside","Dead on the inside, dead on the outside","When you play a game of thrones you win or you die.","Your mother was worried sick and I was here drinking beer"]
instance Arbitrary (Realistic Body) where
  arbitrary = Realistic . Body <$> genRealisticText

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic Tag)
-- ["Cloned","stygian","expansion play","mission-critical","fainted","tofu","crucifix","get a pulse on","penetrate the market","Function-based","small batch"]
instance Arbitrary (Realistic Tag) where
  arbitrary = Realistic . Tag <$> genRealisticBuzzword

instance Arbitrary (Realistic a) => Arbitrary (Realistic [a]) where
  arbitrary = Realistic <$> (getRealistic <<$>> listOf arbitrary)
  shrink (Realistic xs) = Realistic <$> (getRealistic <<$>> shrink (Realistic <$> xs))

-- | @since 0.2.0.0
-- >>> sample' $ arbitrary @(Realistic (AuthOf 'User))
-- [UserAuth {email = "erlich@bachmanity.test", username = "Ben Lyon", bio = "For ever and a day.", image = "imageTBE"},UserAuth {email = "laurie@raviga.test", username = "Arie Aufderhar Wisozk", bio = " audiences are going to adore your tour de force performance as the forceful denim-clad court reporter in 'The Court Reporter Sported Jorts', the jet-setting jort-sporting court reporter story", image = "imageTBE"},UserAuth {email = "Merry_Okuneva-541711812@Williamson_LLC.net", username = "Hubert Zieme Luettgen", bio = "When you play a game of thrones you win or you die.", image = "imageTBE"},UserAuth {email = "daniel.carey@reynholm.test", username = "Doug Updegrave", bio = "For ever and a day.", image = "imageTBE"},UserAuth {email = "jared@piedpiper.test", username = "Mr. Lorenzo Schaefer Cartwright", bio = "I'm the dude, so that's what you call me. That or, uh His Dudeness, or uh Duder, or El Duderino, if you're not into the whole brevity thing.", image = "imageTBE"},UserAuth {email = "Tommie_Huel-104543348@Franecki_Inc.net", username = "Mable Kerluke", bio = "There is nothing either good or bad, but thinking makes it so.", image = "imageTBE"},UserAuth {email = "Greta_Rice-871679444@Zemlak_LLC.net", username = "Frida Olson Kerluke", bio = "The world is grown so bad that wrens make prey where eagles dare not perch.", image = "imageTBE"},UserAuth {email = "johan@hotmail.test", username = "Janeen Reinger", bio = "Portnoy finds joy in hoi polloi boy toy", image = "imageTBE"},UserAuth {email = "Augustus_Smitham-392056915@Dare_Group.name", username = "Mr. Libby Quitzon Donnelly", bio = "When you play a game of thrones you win or you die.", image = "imageTBE"},UserAuth {email = "erlich@bachmanity.test", username = "Jimmy DeLocke", bio = "Portnoy finds joy in hoi polloi boy toy", image = "imageTBE"},UserAuth {email = "johan@hotmail.test", username = "Mr. Brigid Schaden Champlin", bio = "For ever and a day.", image = "imageTBE"}]
instance Arbitrary (Realistic (AuthOf 'User)) where
  arbitrary = Realistic <$> (UserAuth <$> arbitraryRealistic <*> arbitraryRealistic <*> arbitraryRealistic <*> arbitraryRealistic)
  shrink (Realistic (UserAuth em user bio' im)) =
    Realistic <$> (UserAuth <$> shrinkRealistic em <*> shrinkRealistic user <*> shrinkRealistic bio' <*> shrinkRealistic im)

-- FIXME realistic validated instance?
instance Arbitrary (Realistic Password) where
  arbitrary =
    Realistic . Password
      <$> (mkPassword . fromString <$> (elements [6 .. 15] >>= flip replicateM arbitraryASCIIChar))
        -- <$> (mkPassword <$> arbitrary)
        -- <$> (mkPassword . fromString <$> listOf arbitraryASCIIChar)
        `suchThat` (\pw -> validatePassword defaultPasswordPolicy_ pw == ValidPassword)

instance Arbitrary (Realistic a) => Arbitrary (Realistic (In a)) where
  arbitrary = arbitrary >>= \(Realistic a) -> pure (Realistic (In a))

instance Arbitrary (Realistic (LoginOf 'User)) where
  arbitrary = Realistic <$> (UserLogin <$> arbitraryRealistic <*> arbitraryRealistic)
  shrink (Realistic (UserLogin em pw)) = Realistic <$> (UserLogin <$> shrinkRealistic em <*> shrinkRealistic pw)

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

instance Arbitrary (Realistic a) => Arbitrary (Realistic (SG.Last a)) where
  arbitrary = Realistic . SG.Last <$> arbitraryRealistic

instance Arbitrary (Realistic a) => Arbitrary (Realistic (Maybe a)) where
  arbitrary = Realistic <$> frequency [(1, Just <$> arbitraryRealistic @a), (1, pure Nothing)]

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

instance Arbitrary (Realistic (IdOf 'User)) where
  arbitrary = Realistic . UserId <$> arbitraryRealistic

deriving newtype instance Arbitrary (Realistic Time)

instance Arbitrary (Realistic (Patch (UpdateOf 'Article))) where
  arbitrary =
    Realistic
      <$> construct
        ( build @(Patch (UpdateOf 'Article))
            arbitraryRealistic
            arbitraryRealistic
            arbitraryRealistic
        )
