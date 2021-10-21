{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Storage.Map.Internal.HasCreate where

import GHC.TypeLits (Symbol)

class HasCreate (s :: Symbol) where
  data CreateOf s
