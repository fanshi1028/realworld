{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Storage.Map.HasId.Ord where

import Data.Domain (Domain (..))
import Data.Storage.Map (IdOf (..))
import Orphans.Internal.Data.Field.Slug.Ord ()
import Orphans.Internal.Data.Field.Username.Ord ()

-- for state machine, Set in model

deriving instance Ord (IdOf 'User)

deriving instance Ord (IdOf 'Article)

deriving instance Ord (IdOf 'Comment)
