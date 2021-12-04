{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Email of User Relation
--
-- @since 0.3.0.0
module Relation.Internal.ToOne.EmailOfUser where

import Domain (Domain (User))
import Field.Email (Email)
import Relation.Internal.ToOne (ToOne (..))
import Storage.Map (IdOf)

-- | since 0.3.0.0
instance ToOne "EmailOfUser" where
  type ToOneKey "EmailOfUser" = Email
  type ToOneValue "EmailOfUser" = IdOf 'User
