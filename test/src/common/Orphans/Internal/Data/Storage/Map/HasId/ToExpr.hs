{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Storage.Map.HasId.ToExpr where

import Data.Domain (Domain (..))
import Data.Storage.Map (IdOf (..))
import Orphans.Internal.Data.Field.Email.ToExpr ()
import Orphans.Internal.Data.Field.Slug.ToExpr ()
import Orphans.Internal.Data.Field.Username.ToExpr ()
import Test.StateMachine (ToExpr)

deriving newtype instance ToExpr (IdOf 'Article)

deriving newtype instance ToExpr (IdOf 'Comment)

deriving newtype instance ToExpr (IdOf 'User)
