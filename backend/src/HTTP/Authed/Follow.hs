{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Authed.Follow (FollowApi, followServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Domain.User (UserR (..))
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (ToggleApi)
import Servant (Capture, ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified UserAction

type FollowApi = Capture "username" (UserR "id") :> "follow" :> ToggleApi UserR "profile"

followServer ::
  ( Algebra sig m,
    Member UserAction.E sig
  ) =>
  ServerT FollowApi m
followServer uid =
  (Out <$> send (UserAction.FollowUser uid))
    :<|> (Out <$> send (UserAction.UnfollowUser uid))
