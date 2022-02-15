{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : Typeclass
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Type family for updating in storage
--
-- @since 0.4.0.0
module Data.Storage.Map.Internal.HasUpdate where

import Data.Aeson (Key, Object)
import Data.Aeson.Types (Parser)
import Data.Domain (Domain)
import Data.Generic.HKD (HKD)
import qualified Data.Semigroup as SG (Last)
import Data.Util.JSON.From (acceptOnlyKeys)

-- | @since 0.3.0.0
class HasUpdate (s :: Domain) where
  -- | @since 0.3.0.0
  -- Type of update
  data UpdateOf s

-- | @since 0.3.0.0
-- An update patch (using with "Data.Generic.HKD")
type Patch a = HKD (HKD a SG.Last) Maybe

-- | @since 0.4.0.0
updatableKeys ::
  -- | List of keys that can be updated
  [Key] ->
  -- | Object containing new values of fields to be updated to
  Object ->
  Parser ()
updatableKeys keys = acceptOnlyKeys keys $ "Only keys " ++ show keys ++ " are updatable, while we found other keys: "
