{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.1.0
module Orphans.Internal.Data.Field.Title.ToExpr where

import Data.Field.Title (Title (Title))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Title
