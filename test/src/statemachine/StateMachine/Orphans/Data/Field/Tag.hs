{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.1.0
module StateMachine.Orphans.Data.Field.Tag where

import Data.Field.Tag (Tag (Tag))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Tag
