{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Field.Image.ToExpr where

import Data.Field.Image (Image (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Image
