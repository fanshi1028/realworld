{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Email where

import Data.Field.Email (Email (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Text ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Email
-- ["","\US\1045981","\GS","","_\1055675\53519XI+","\SUB_c\1067638(RD\t9","e\198568","G\65252r]\nD\185163w\1029797\ETXQ\92989\1091823\1022806","M","Y\172713\1069130\1053094r","~I\1019569d\CAN"]
deriving instance Arbitrary Email
