{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Email of User Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ToOne.EmailOfUser where

import Data.Domain (Domain (User))
import Data.Field.Email (Email)
import Data.Storage.Map (IdOf)
import InMem.Relation.Internal.ToOne (ToOne (..))

-- | @since 0.3.0.0
data EmailOfUser

-- | since 0.3.0.0
instance ToOne EmailOfUser where
  type ToOneKey EmailOfUser = Email
  type ToOneValue EmailOfUser = IdOf 'User
