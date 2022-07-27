{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StateMachine.Orphans.Data.Storage.Map.HasStorage where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasStorage (IdOf (..))
import StateMachine.Orphans.Data.Field.Email ()
import StateMachine.Orphans.Data.Field.Slug ()
import StateMachine.Orphans.Data.Field.Username ()
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr (IdOf 'Article)

deriving newtype instance ToExpr (IdOf 'Comment)

deriving newtype instance ToExpr (IdOf 'User)
