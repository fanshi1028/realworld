{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public (PublicApi, publicServer) where

import Control.Algebra (Algebra)
import Control.Effect.Sum (Member)
import HTTP.Public.Article (ArticleApi, articleServer)
import HTTP.Public.Profile (ProfileApi, profileServer)
import HTTP.Public.Tag (TagApi, tagServer)
import HTTP.Public.User (UserApi, userServer)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import Tag.Effect (Tag)
import VisitorAction.Effect (VisitorAction)

type PublicApi =
  "users" :> UserApi
    :<|> "profiles" :> ProfileApi
    :<|> "articles" :> ArticleApi
    :<|> "tags" :> TagApi

publicServer ::
  ( Algebra sig m,
    Member VisitorAction sig,
    Member (Tag []) sig
  ) =>
  ServerT PublicApi m
publicServer = userServer :<|> profileServer :<|> articleServer :<|> tagServer
