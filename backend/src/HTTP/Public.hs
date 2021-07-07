{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public (PublicApi, publicServer) where

import Domain.Util.Field (Tag)
import Domain.Util.JSON.To (Out)
import HTTP.Public.Article (ArticleApi, articleServer)
import HTTP.Public.Profile (ProfileApi, profileServer)
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
  (TagServerEffect sig m) =>
  EffRunner m (Out [Tag]) ->
  Server PublicApi
publicServer tagC = userServer :<|> profileServer :<|> articleServer :<|> tagServer tagC
