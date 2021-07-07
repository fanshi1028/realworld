{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public (PublicApi, publicServer) where

import Domain.User (UserR)
import Domain.Util.Field (Tag)
import Domain.Util.JSON.To (Out)
import HTTP.Public.Article (ArticleApi, articleServer)
import HTTP.Public.Profile (ProfileApi, ProfileServerEffect, profileServer)
import HTTP.Public.Tag (TagApi, TagServerEffect, tagServer)
import HTTP.Public.User (UserApi, userServer)
import HTTP.Util (EffRunner)
import Servant (Server, type (:<|>) ((:<|>)), type (:>))

type PublicApi =
  "users" :> UserApi
    :<|> "profiles" :> ProfileApi
    :<|> "articles" :> ArticleApi
    :<|> "tags" :> TagApi

publicServer ::
  (ProfileServerEffect sig1 m, TagServerEffect sig2 n) =>
  EffRunner m (Out (UserR "profile")) ->
  EffRunner n (Out [Tag]) ->
  Server PublicApi
publicServer profileC tagC = userServer :<|> profileServer profileC :<|> articleServer :<|> tagServer tagC
