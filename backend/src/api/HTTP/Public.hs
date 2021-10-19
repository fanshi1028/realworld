{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Public API & Server, for visitors, or optionally-authed.
--
-- @since 0.1.0.0
module HTTP.Public where

import Control.Algebra (Algebra)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Util.Error (ValidationErr)
import HTTP.Public.Article (ArticleApi, articleServer)
import HTTP.Public.Profile (ProfileApi, profileServer)
import HTTP.Public.Tag (TagApi, tagServer)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified VisitorAction (E)

-- * API

-- | @since 0.1.0.0
type PublicApi =
  "profiles" :> ProfileApi
    :<|> "articles" :> ArticleApi
    :<|> "tags" :> TagApi

-- * Server

-- | @since 0.1.0.0
publicServer ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT PublicApi m
publicServer = profileServer :<|> articleServer :<|> tagServer
