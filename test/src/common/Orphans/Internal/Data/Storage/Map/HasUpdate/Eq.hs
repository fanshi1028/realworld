{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Storage.Map.HasUpdate.Eq where

import Data.Domain (Domain (..))
import Data.Storage.Map (UpdateOf (..))
import Orphans.Internal.Data.Field.Password.Eq ()

deriving instance Eq (UpdateOf 'User)
