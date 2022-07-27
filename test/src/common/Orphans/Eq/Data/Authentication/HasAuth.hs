{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Eq.Data.Authentication.HasAuth where

import Data.Authentication.HasAuth (LoginOf (..))
import Data.Domain (Domain (..))
import Orphans.Eq.Data.Field.Password ()

deriving instance Eq (LoginOf 'User)
