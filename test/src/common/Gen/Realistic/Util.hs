{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gen.Realistic.Util where

import qualified Data.Semigroup as SG
import Faker (Fake)
import qualified Faker.Book as Book
import qualified Faker.Book.CultureSeries as CultureSeries
import qualified Faker.Book.Dune as Dune
import qualified Faker.Book.Lovecraft as Lovecraft
import qualified Faker.Cannabis as Cannabis
import Faker.Combinators (oneof)
import qualified Faker.Company as Company
import qualified Faker.Game.Witcher as Witcher
import qualified Faker.GreekPhilosophers as GreekPhilosophers
import qualified Faker.Hipster as Hipster
import qualified Faker.JapaneseMedia.OnePiece as OnePiece
import qualified Faker.Marketing as Marketing
import qualified Faker.Movie.HarryPotter as HarryPotter
import qualified Faker.Movie.Lebowski as Lebowski
import qualified Faker.Movie.VForVendetta as VForVendetta
import qualified Faker.Quote.Shakespeare as Shakespeare
import qualified Faker.TvShow.BoJackHorseman as BojackHorseman
import qualified Faker.TvShow.GameOfThrones as GameOfThrones
import qualified Faker.TvShow.SouthPark as SouthPark
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, frequency, listOf)
import Test.QuickCheck.Gen.Faker (fakeQuickcheck)

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

instance Arbitrary (Realistic a) => Arbitrary (Realistic [a]) where
  arbitrary = Realistic <$> (getRealistic <<$>> listOf arbitrary)
  shrink (Realistic xs) = Realistic <$> (getRealistic <<$>> shrink (Realistic <$> xs))

instance Arbitrary (Realistic a) => Arbitrary (Realistic (SG.Last a)) where
  arbitrary = Realistic . SG.Last <$> arbitraryRealistic

instance Arbitrary (Realistic a) => Arbitrary (Realistic (Maybe a)) where
  arbitrary = Realistic <$> frequency [(1, Just <$> arbitraryRealistic @a), (1, pure Nothing)]
