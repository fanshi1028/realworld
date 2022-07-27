{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Eq.Data.Storage.Map.HasUpdate where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasUpdate (UpdateOf (..))
import Orphans.Eq.Data.Field.Password ()

deriving instance Eq (UpdateOf 'User)
