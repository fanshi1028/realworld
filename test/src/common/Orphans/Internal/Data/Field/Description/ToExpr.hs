{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.1.0
module Orphans.Internal.Data.Field.Description.ToExpr where

import Data.Field.Description (Description (Description))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Description
