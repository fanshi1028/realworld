{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

import Data.Domain (Domain (Article, User))
import Data.Storage.Map (IdOf)
import InMem.Relation.Internal.ToMany (ToMany (..))

-- | @since 0.3.0.0
data UserCreateArticle

-- | @since 0.3.0.0
instance ToMany UserCreateArticle where
  type ToManyKey UserCreateArticle = IdOf 'User
  type ToManyValue UserCreateArticle = IdOf 'Article
