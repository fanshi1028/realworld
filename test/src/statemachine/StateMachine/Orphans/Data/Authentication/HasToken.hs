{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StateMachine.Orphans.Data.Authentication.HasToken where

import Data.Authentication.HasToken (TokenOf (..))
import Data.Domain (Domain (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr (TokenOf 'User)
