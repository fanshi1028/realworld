{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StateMachine.Orphans.Data.Field.Image where

import Data.Field.Image (Image (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Image
