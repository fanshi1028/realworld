{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Instance
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- User create Article Relation
--
-- @since 0.3.0.0
module InMem.Relation.Internal.ToMany.UserCreateArticle where

import Domain (Domain (Article, User))
import InMem.Relation.Internal.ToMany (ToMany (..))
import InMem.Storage.Map (IdOf)

-- | @since 0.3.0.0
instance ToMany "UserCreateArticle" where
  type ToManyKey "UserCreateArticle" = IdOf 'User
  type ToManyValue "UserCreateArticle" = IdOf 'Article
