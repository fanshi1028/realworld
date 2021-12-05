{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Type family for updating in storage
--
-- @since 0.2.0.0
module InMem.Storage.Map.Internal.HasUpdate where

import Data.Aeson (Object)
import Data.Aeson.Types (Parser)
import Data.Generic.HKD (HKD)
import qualified Data.Semigroup as SG (Last)
import Domain (Domain)
import Util.JSON.From (acceptOnlyKeys)

-- | @since 0.2.0.0
class HasUpdate (s :: Domain) where
  -- | @since 0.2.0.0
  -- Type of update
  data UpdateOf s

-- | @since 0.2.0.0
-- An update patch (using with "Data.Generic.HKD")
type Patch a = HKD (HKD a SG.Last) Maybe

-- | @since 0.2.0.0
updatableKeys ::
  -- | List of keys that can be updated
  [Text] ->
  -- | Object containing new values of fields to be updated to
  Object ->
  Parser ()
updatableKeys keys = acceptOnlyKeys keys $ "Only keys " ++ show keys ++ " are updatable, while we found other keys: "
