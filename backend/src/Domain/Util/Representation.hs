{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Transforms between different representations of Domain Types.
--
-- @since 0.1.0.0
module Util.Representation (Transform (transform)) where

import Authentication (AuthOf (UserAuth))
import Storage.Map (ContentOf (..), IdOf, toArticleId, toUserId)
import Storage.Map.Internal.HasStorage.User ()
import GHC.Records (getField)

-- | Transform between different representation of the same data
--
-- @since 0.2.0.0
class Transform a b where
  transform :: a -> b

-- | @since 0.2.0.0
instance Transform (ContentOf "user") (AuthOf "user") where
  transform (UserContent em _ name bio' img) = UserAuth em name bio' img

instance Transform (ContentOf "user") (IdOf "user") where
  transform = toUserId

instance Transform (ContentOf "article") (IdOf "article") where
  transform = toArticleId

instance Transform (ContentOf "comment") (IdOf "comment") where
  transform = getField @"id"
