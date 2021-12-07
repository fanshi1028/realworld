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

import Domain (Domain (User))
import Field.Email (Email)
import InMem.Relation.Internal.ToOne (ToOne (..))
import Storage.Map (IdOf)

-- | since 0.3.0.0
instance ToOne "EmailOfUser" where
  type ToOneKey "EmailOfUser" = Email
  type ToOneValue "EmailOfUser" = IdOf 'User
