{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Ord.Data.Storage.Map.HasStorage where

import Data.Domain (Domain (..))
import Data.Storage.Map.HasStorage (IdOf (..))
import Orphans.Ord.Data.Field.Slug ()
import Orphans.Ord.Data.Field.Username ()

-- for state machine, Set in model

deriving instance Ord (IdOf 'User)

deriving instance Ord (IdOf 'Article)

deriving instance Ord (IdOf 'Comment)
