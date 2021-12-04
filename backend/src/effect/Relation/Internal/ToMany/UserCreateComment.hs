{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Relation.Internal.ToMany.UserCreateComment where

import Domain (Domain (Comment, User))
import Relation.Internal.ToMany (ToMany (..))
import Storage.Map (IdOf)

-- | @since 0.3.0.0
instance ToMany "UserCreateComment" where
  type ToManyKey "UserCreateComment" = IdOf 'User
  type ToManyValue "UserCreateComment" = IdOf 'Comment
