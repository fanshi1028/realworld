{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.Profile (ProfileApi, profileServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Domain.User (UserR)
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (ReadApi)
import Servant (Capture, ServerT, type (:>))
import VisitorAction.Effect (VisitorAction (GetProfile))

type ProfileApi = Capture "username" (UserR "id") :> ReadApi UserR "profile"

profileServer :: (Member VisitorAction sig, Algebra sig m) => ServerT ProfileApi m
profileServer uid = Out <$> send (GetProfile uid)
