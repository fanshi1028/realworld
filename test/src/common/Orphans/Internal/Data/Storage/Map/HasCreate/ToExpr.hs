{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @sicne 0.4.1.0
module Orphans.Internal.Data.Storage.Map.HasCreate.ToExpr where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasCreate (CreateOf (..))
import Orphans.Internal.Data.Field.Body.ToExpr ()
import Orphans.Internal.Data.Field.Description.ToExpr ()
import Orphans.Internal.Data.Field.Title.ToExpr ()
import Orphans.Internal.Data.Field.Tag.ToExpr ()
import Orphans.Internal.Data.Field.Username.ToExpr ()
import Orphans.Internal.Data.Field.Email.ToExpr ()
import Orphans.Internal.Data.Field.Password.ToExpr ()
import Test.StateMachine (ToExpr)

instance ToExpr (CreateOf 'User)

instance ToExpr (CreateOf 'Article)

instance ToExpr (CreateOf 'Comment)
