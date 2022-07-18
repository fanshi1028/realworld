{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Tag where

import Data.Field.Tag (Tag (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Text ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Tag
-- ["","N]","9\1012553","w\1016665\&5EK","Z3G!z\SOH+\1027100","\64959\DC3K\FSj\v\a\1039450@\1071227","\155137\&1\r","H\995424<Z\1105346\20322\EOT\DC4\39312`=","I\1031729\1008231uI","\ETBe>,k%\NAK\EOTE;8d\990470\78144","X-\DC2\60369\&6p(\DLEo=E\4320\DELl${["]
deriving instance Arbitrary Tag
