{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Authentication.Internal.HasAuth where

import GHC.TypeLits (Symbol)

-- | @since 0.2.0.0
class HasAuth (s :: Symbol) where
  data LoginOf s
  data AuthOf s
