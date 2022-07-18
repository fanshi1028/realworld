{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Description where

import Data.Field.Description (Description (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Text ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Description
-- ["","\v\ETX","\98655","","*j\1086030\24957,V",":","\98136\187630\1027868","\111228e\US&b4<}\aAfu","]T-kN]\DC1lmXz%","6\62613wyh\DC1\a\GS]t\GS\1106069r)","\CAN%\NUL\175015\28016w\DLEy\987128"]
deriving instance Arbitrary Description
