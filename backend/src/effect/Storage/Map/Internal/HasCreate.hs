{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- |
module Storage.Map.Internal.HasCreate where

import Domain (Domain)

class HasCreate (s :: Domain) where
  data CreateOf s
