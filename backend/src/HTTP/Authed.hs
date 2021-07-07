{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Authed (AuthedApi, authedServer) where

import Control.Algebra (Algebra)
import Control.Effect.Sum (Member)
import Domain.User (UserR)
import Domain.Util.JSON.To (Out)
import HTTP.Authed.Article (ArticleApi, articleServer)
import HTTP.Authed.Follow (FollowApi, followServer)
import HTTP.Authed.User (UserApi, userServer)
import HTTP.Util (EffRunner)
import Servant (Server, type (:<|>) ((:<|>)), type (:>))
import UserAction.Effect (UserAction)

type AuthedApi =
  "user" :> UserApi
    :<|> "profiles" :> FollowApi
    :<|> "articles" :> ArticleApi

authedServer ::
  -- (Algebra sig m, Member UserAction sig) =>
  -- EffRunner m (Out (UserR "profile")) ->
  Server AuthedApi
-- authedServer profileC = userServer :<|> profileServer profileC :<|> articleServer
authedServer = userServer :<|> followServer :<|> articleServer
