{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.1.0
module StateMachine.Orphans.Data.Field.Bio where

import Data.Field.Bio (Bio (Bio))
import Test.StateMachine (ToExpr)

-- | @since 0.4.1.0
deriving newtype instance ToExpr Bio
