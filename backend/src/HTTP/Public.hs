{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public (PublicApi, publicServer) where

import Control.Algebra (Algebra)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw)
import Domain.Util.Error (ValidationErr)
import HTTP.Public.Article (ArticleApi, articleServer)
import HTTP.Public.Profile (ProfileApi, profileServer)
import HTTP.Public.Tag (TagApi, tagServer)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified VisitorAction (E)

type PublicApi =
  "profiles" :> ProfileApi
    :<|> "articles" :> ArticleApi
    :<|> "tags" :> TagApi

publicServer ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT PublicApi m
publicServer = profileServer :<|> articleServer :<|> tagServer
