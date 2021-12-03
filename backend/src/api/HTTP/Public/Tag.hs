{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server to read all tags
--
-- @since 0.1.0.0
module HTTP.Public.Tag where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Field.Tag (Tag)
import Servant (Get, JSON, ServerT)
import Util.JSON.To (Out (Out))
import VisitorAction (VisitorActionE (GetTags))

-- * API

-- | @since 0.1.0.0
type TagApi = Get '[JSON] (Out [Tag])

-- * Server

-- | @since 0.1.0.0
tagServer :: (Member VisitorActionE sig, Algebra sig m) => ServerT TagApi m
tagServer = Out <$> send GetTags
