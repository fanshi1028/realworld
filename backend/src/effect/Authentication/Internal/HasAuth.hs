{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Authentication.Internal.HasAuth where

import Domain (Domain)

-- | @since 0.2.0.0
class HasAuth (s :: Domain) where
  data LoginOf s
  data AuthOf s
