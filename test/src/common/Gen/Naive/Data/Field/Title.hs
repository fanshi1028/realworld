{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Gen.Naive.Data.Field.Title where

import Data.Field.Title (Title (..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances.Text ()

-- | @since 0.2.0.0
-- >>> import Test.QuickCheck (sample')
-- >>> sample' $ arbitrary @Title
-- ["","\33532","\1082204\ESC","\ENQC}h-J","b","l\1086590\\","\78209:k\ESC","c","-~\1069271\&7n!\199009}\30571B","!\68081re\\\"Y~","\RSq;"]
deriving instance Arbitrary Title
