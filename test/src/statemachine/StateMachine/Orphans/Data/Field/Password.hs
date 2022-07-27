{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.1.0
module StateMachine.Orphans.Data.Field.Password where

import Data.Field.Password (Password (..))
import Test.StateMachine (ToExpr (toExpr))

instance ToExpr Password where
  toExpr = toExpr . show @Text
