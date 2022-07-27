{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @sicne 0.4.1.0
module StateMachine.Orphans.Data.Storage.Map.HasCreate where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasCreate (CreateOf (..))
import StateMachine.Orphans.Data.Field.Body ()
import StateMachine.Orphans.Data.Field.Description ()
import StateMachine.Orphans.Data.Field.Email ()
import StateMachine.Orphans.Data.Field.Password ()
import StateMachine.Orphans.Data.Field.Tag ()
import StateMachine.Orphans.Data.Field.Title ()
import StateMachine.Orphans.Data.Field.Username ()
import Test.StateMachine (ToExpr)

instance ToExpr (CreateOf 'User)

instance ToExpr (CreateOf 'Article)

instance ToExpr (CreateOf 'Comment)
