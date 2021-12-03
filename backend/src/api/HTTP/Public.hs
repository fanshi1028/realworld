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

import Control.Algebra (Algebra)
import Control.Effect.Sum (Member)
import HTTP.Public.Tag (TagApi, tagServer)
import Servant (ServerT, type (:>))
import VisitorAction (VisitorActionE)

-- * API

-- | @since 0.3.0.0
type PublicApi = "tags" :> TagApi

-- * Server

-- | @since 0.3.0.0
publicServer ::
  ( Algebra sig m,
    Member VisitorActionE sig
  ) =>
  ServerT PublicApi m
publicServer = tagServer
