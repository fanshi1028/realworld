{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Field.Slug.ToExpr where

import Data.Field.Slug (Slug (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr Slug
