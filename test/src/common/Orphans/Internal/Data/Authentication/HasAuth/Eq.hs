{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Internal.Data.Authentication.HasAuth.Eq where

import Data.Authentication.HasAuth (LoginOf (..))
import Data.Domain (Domain (..))
import Orphans.Internal.Data.Field.Password.Eq ()

deriving instance Eq (LoginOf 'User)
