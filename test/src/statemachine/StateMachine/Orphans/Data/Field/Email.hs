{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StateMachine.Orphans.Data.Field.Email where

import Data.Field.Email (Email (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Email
