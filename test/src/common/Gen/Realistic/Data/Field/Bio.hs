{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Realistic.Data.Field.Bio where

import Data.Field.Bio (Bio (..))
import Gen.Realistic.Util (Realistic (..), genRealisticText)
import Test.QuickCheck (Arbitrary (arbitrary))

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @(Realistic Bio)
-- ["See how she leans her cheek upon her hand. Oh, that I were a glove upon that hand that I might touch that cheek!","When you play a game of thrones you win or you die.","The world is grown so bad that wrens make prey where eagles dare not perch.","The world is grown so bad that wrens make prey where eagles dare not perch.","You know what they say: You can't teach a gay dog straight tricks","The world is grown so bad that wrens make prey where eagles dare not perch.","Philosophy is the highest music.","Dead on the inside, dead on the outside","There is nothing either good or bad, but thinking makes it so.","I'm the dude, so that's what you call me. That or, uh His Dudeness, or uh Duder, or El Duderino, if you're not into the whole brevity thing.","Laughter is poison to fear."]
instance Arbitrary (Realistic Bio) where
  arbitrary = Realistic . Bio <$> genRealisticText
