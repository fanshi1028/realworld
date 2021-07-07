{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Authed.Follow (FollowApi, followServer) where

import Domain.User (UserR)
import HTTP.Util (ToggleApi)
import Servant (Capture, Server, type (:>))

type FollowApi = Capture "username" (UserR "id") :> "follow" :> ToggleApi UserR "profile"

-- FIXME
followServer ::
  -- (Algebra sig m, Member UserAction sig) =>
  -- EffRunner m (Out (UserR "profile")) ->
  Server FollowApi
-- followServer carrier uid =
--   pure (run $ carrier $ Out <$> send (FollowUser uid))
--     :<|> pure (run $ carrier $ Out <$> send (UnfollowUser uid))
followServer uid = undefined
