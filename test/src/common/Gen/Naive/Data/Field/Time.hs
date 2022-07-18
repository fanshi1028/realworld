{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Time where

import Data.Field.Time (Time (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Time ()

-- | @since 0.3.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Time
-- [1864-05-09 12:15:24.478938341373 UTC,1864-05-08 04:14:22.787283774678 UTC,1864-05-11 20:11:07.350399947405 UTC,1864-05-04 16:02:04.890488921402 UTC,1864-05-11 23:30:49.330851050355 UTC,1864-05-13 13:26:30.361190437026 UTC,1864-04-30 19:23:16.659840504304 UTC,1864-05-15 02:33:42.833714440686 UTC,1864-05-25 09:04:52.111442215958 UTC,1864-04-24 04:48:10.953480851738 UTC,1864-05-09 06:21:57.673382779961 UTC]
deriving instance Arbitrary Time
