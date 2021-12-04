{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Relation.Internal.ToMany.UserCreateArticle where

import Domain (Domain (Article, User))
import Relation.Internal.ToMany (ToMany (..))
import Storage.Map (IdOf)

-- | @since 0.3.0.0
instance ToMany "UserCreateArticle" where
  type ToManyKey "UserCreateArticle" = IdOf 'User
  type ToManyValue "UserCreateArticle" = IdOf 'Article
