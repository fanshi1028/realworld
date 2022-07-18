{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Body where

import Data.Field.Body (Body (..))
import Gen.Realistic.Util (Realistic (..), genRealisticText)
import Test.QuickCheck (Arbitrary (arbitrary))

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(Realistic Body)
-- ["A lion doesn't concern itself with the opinion of sheep.","A few vices are sufficient to darken many virtues.","See how she leans her cheek upon her hand. Oh, that I were a glove upon that hand that I might touch that cheek!","I wish I wasn't afraid all the time, but... I am.","Stop counting only those things you have lost! What is gone, is gone! So ask yourself this. What is there... that still remains to you?!","Hey, I know that guy. He's a nihilist. Karl Hungus.","Dead on the inside, dead on the outside","Dead on the inside, dead on the outside","Dead on the inside, dead on the outside","When you play a game of thrones you win or you die.","Your mother was worried sick and I was here drinking beer"]
instance Arbitrary (Realistic Body) where
  arbitrary = Realistic . Body <$> genRealisticText
