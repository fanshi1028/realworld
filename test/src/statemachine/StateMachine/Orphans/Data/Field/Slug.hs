{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StateMachine.Orphans.Data.Field.Slug where

import Data.Field.Slug (Slug (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Slug
