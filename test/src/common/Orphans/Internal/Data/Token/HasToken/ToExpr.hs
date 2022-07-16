{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Token.HasToken.ToExpr where

import Data.Domain (Domain (..))
import Data.Token.HasToken (TokenOf (..))
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr (TokenOf 'User)
