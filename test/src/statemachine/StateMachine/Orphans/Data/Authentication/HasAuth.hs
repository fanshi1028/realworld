{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module StateMachine.Orphans.Data.Authentication.HasAuth where

import Data.Authentication.HasAuth (AuthOf)
import Data.Domain (Domain (..))
import StateMachine.Orphans.Data.Field.Bio ()
import StateMachine.Orphans.Data.Field.Email ()
import StateMachine.Orphans.Data.Field.Image ()
import StateMachine.Orphans.Data.Field.Username ()
import Test.StateMachine (ToExpr)

instance ToExpr (AuthOf 'User)
