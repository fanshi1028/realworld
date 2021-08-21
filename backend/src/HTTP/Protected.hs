{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Protected (AuthedApi, authedServer) where

import Control.Algebra (Algebra)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain.Util.Error (ValidationErr)
import HTTP.Protected.Article (ArticleApi, articleServer)
import HTTP.Protected.Follow (FollowApi, followServer)
import HTTP.Protected.User (UserApi, userServer)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified UserAction (E)

type AuthedApi =
  "user" :> UserApi
    :<|> "profiles" :> FollowApi
    :<|> "articles" :> ArticleApi

authedServer ::
  ( Algebra sig m,
    Member UserAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT AuthedApi m
authedServer = userServer :<|> followServer :<|> articleServer
