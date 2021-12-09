{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server, optional authed.
--
-- @since 0.3.0.0
module HTTP.OptionalAuth where

import Control.Algebra (Algebra)
import qualified Control.Effect.Reader as R (Reader)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import HTTP.OptionalAuth.Article (ArticleApi, articleServer)
import HTTP.OptionalAuth.Profile (ProfileApi, profileServer)
import OptionalAuthAction (OptionalAuthActionE)
import Paging (Limit, Offset)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import Util.Validation (ValidationErr)

-- * API

-- | @since 0.3.0.0
type OptionallyAuthedApi =
  "profiles" :> ProfileApi
    :<|> "articles" :> ArticleApi

-- * Server

-- | @since 0.3.0.0
optionallyAuthedServer ::
  ( Algebra sig m,
    Member OptionalAuthActionE sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT OptionallyAuthedApi m
optionallyAuthedServer = profileServer :<|> articleServer
