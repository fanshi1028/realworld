{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Public API & Server, for visitors.
--
-- @since 0.1.0.0
module HTTP.Public where

import Field.Tag (Tag)
import HTTP.Util (ReadManyApi)
import Servant (type (:>))

-- * API

-- | @since 0.3.0.0
type PublicTagApi = ReadManyApi Tag

-- | @since 0.3.0.0
type PublicApi = "tags" :> PublicTagApi
