{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @sicne 0.4.1.0

module Orphans.Internal.Data.Storage.Map.HasCreate.Eq where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasCreate (CreateOf (..))
import Orphans.Internal.Data.Field.Password.Eq ()

deriving instance Eq (CreateOf 'User)

deriving instance Eq (CreateOf 'Article)

deriving instance Eq (CreateOf 'Comment)
