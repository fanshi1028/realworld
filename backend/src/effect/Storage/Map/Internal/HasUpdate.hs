{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
module Storage.Map.Internal.HasUpdate where

import Data.Aeson (Object)
import Data.Aeson.Types (Parser)
import Data.Generic.HKD (HKD)
import qualified Data.Semigroup as SG (Last)
import Domain (Domain)
import Util.JSON.From (acceptOnlyKeys)

class HasUpdate (s :: Domain) where
  data UpdateOf s

type Patch a = HKD (HKD a SG.Last) Maybe

updatableKeys :: [Text] -> Object -> Parser ()
updatableKeys keys = acceptOnlyKeys keys $ "Only keys " ++ show keys ++ " are updatable, while we found other keys: "
