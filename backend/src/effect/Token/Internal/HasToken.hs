{-# LANGUAGE TypeFamilies #-}

-- |
module Token.Internal.HasToken where

import Authentication (HasAuth)

-- | @since 0.2.0.0
class HasAuth s => HasToken s where
  data TokenOf s
