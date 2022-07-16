{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Authentication.HasAuth.ToExpr where

import Data.Authentication.HasAuth (AuthOf)
import Data.Domain (Domain (..))
import Orphans.Internal.Data.Field.Bio.ToExpr ()
import Orphans.Internal.Data.Field.Email.ToExpr ()
import Orphans.Internal.Data.Field.Image.ToExpr ()
import Orphans.Internal.Data.Field.Username.ToExpr ()
import Test.StateMachine (ToExpr)

instance ToExpr (AuthOf 'User)
