{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Authed (AuthedApi, authedServer) where

import HTTP.Authed.Article (ArticleApi, articleServer)
import HTTP.Authed.Follow (FollowApi, followServer)
import HTTP.Authed.User (UserApi, userServer)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))

type AuthedApi =
  "user" :> UserApi
    :<|> "profiles" :> FollowApi
    :<|> "articles" :> ArticleApi

authedServer :: ServerT AuthedApi m
authedServer = userServer :<|> followServer :<|> articleServer
