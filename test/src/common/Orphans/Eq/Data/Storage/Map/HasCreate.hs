{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | @sicne 0.4.1.0
module Orphans.Eq.Data.Storage.Map.HasCreate where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasCreate (CreateOf (..))
import Orphans.Eq.Data.Field.Password ()

deriving instance Eq (CreateOf 'User)

deriving instance Eq (CreateOf 'Article)

deriving instance Eq (CreateOf 'Comment)
