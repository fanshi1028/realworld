{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Username where

import Data.Field.Username (Username (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Text ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Username
-- ["",".","\DLE\tv\1034026","","\1011336\45919\31355/ CF\136841","0\176300\SUB\t\DC4uGb\1059101\&7","Cl\9463\&4\CAN\50560\1068046","\97256\1041103(\ACK,","\DLEA\a\EM{7\a^&\ACK\1054374\&4k\DC4\19325&","\n7m\r\1030889'\151710R|\1033552b>l\GS\151713r\SYN","6!\SOo\144149y\SYN\36267\&2vi%=By:\131925|\SO\ETX"]
deriving instance Arbitrary Username
