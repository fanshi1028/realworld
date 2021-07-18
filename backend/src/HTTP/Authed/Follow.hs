{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Authed.Follow (FollowApi, followServer) where

import Domain.User (UserR)
import HTTP.Util (ToggleApi)
import Servant (Capture, ServerT, type (:>))

type FollowApi = Capture "username" (UserR "id") :> "follow" :> ToggleApi UserR "profile"

-- FIXME
followServer :: ServerT FollowApi m
followServer = undefined
