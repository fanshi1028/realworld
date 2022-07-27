{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.1.0
module StateMachine.Orphans.Data.Field.Username where

import Data.Field.Username (Username (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Username
