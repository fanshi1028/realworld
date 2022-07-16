{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Field.Email.ToExpr where

import Data.Field.Email (Email (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Email
