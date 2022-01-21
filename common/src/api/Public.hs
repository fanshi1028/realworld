{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Public API for visitors.
--
-- @since 0.4.0.0
module API.Public where

import API.Util (ReadManyApi)
import Data.Field.Tag (Tag)
import Servant (type (:>))

-- * API

-- | @since 0.3.0.0
type PublicTagApi = ReadManyApi Tag

-- | @since 0.3.0.0
type PublicApi = "tags" :> PublicTagApi
