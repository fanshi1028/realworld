{-# OPTIONS_GHC -Wno-orphans #-}

-- | @since 0.4.1.0
module Orphans.Internal.Data.Field.Password.ToExpr where

import Data.Field.Password (Password (..))
import Test.StateMachine (ToExpr (toExpr))

instance ToExpr Password where
  toExpr = toExpr . show @Text
