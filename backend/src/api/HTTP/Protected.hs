{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Auth protected API & Server
--
-- @since 0.1.0.0
module HTTP.Protected where

import Control.Algebra (Algebra)
import qualified Control.Effect.Reader as R (Reader)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain (Domain (User))
import HTTP.Protected.Article (ArticleApi, articleServer)
import HTTP.Protected.Follow (FollowApi, followServer)
import HTTP.Protected.User (UserApi, userServer)
import Paging (Limit, Offset)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified Token.Create (E)
import UserAction (UserActionE)
import Util.Validation (ValidationErr)

-- * API

-- | @since 0.1.0.0
type AuthedApi =
  "user" :> UserApi
    :<|> "profiles" :> FollowApi
    :<|> "articles" :> ArticleApi

-- * Server

-- | @since 0.3.0.0
authedServer ::
  ( Algebra sig m,
    Member UserActionE sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig,
    Member (Throw ValidationErr) sig,
    Member (Token.Create.E 'User) sig
  ) =>
  ServerT AuthedApi m
authedServer = userServer :<|> followServer :<|> articleServer
