{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Public.Profile (ProfileApi, profileServer) where

import Domain.User (UserR)
import HTTP.Util (ReadApi)
import Servant (Capture, Server, type (:>))

type ProfileApi = Capture "username" (UserR "id") :> ReadApi UserR "profile"

-- FIXME
profileServer :: Server ProfileApi
profileServer = undefined
