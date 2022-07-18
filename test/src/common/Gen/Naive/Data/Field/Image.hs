{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Image where

import Data.Field.Image (Image (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Text ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Image
-- ["","\v\168113","\ACK","","H\189844\ETB","","\DC2J|\SOH\t\SOQ\1103986\SYN%\188423","^\1101298\1102381{","E\a\SOH\ENQK","oa\157026~yglo\t{~\32949=","]"]
deriving instance Arbitrary Image
