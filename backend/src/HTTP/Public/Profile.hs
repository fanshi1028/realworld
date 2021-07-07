{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.Profile (ProfileApi, profileServer, ProfileServerEffect) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Domain.User (UserR)
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (EffRunner, ReadApi)
import Servant (Capture, Server, type (:>))
import VisitorAction.Effect (VisitorAction (GetProfile))

type ProfileApi = Capture "username" (UserR "id") :> ReadApi UserR "profile"

type ProfileServerEffect sig m = (Member VisitorAction sig, Algebra sig m)

profileServer :: (ProfileServerEffect sig m) => EffRunner m (Out (UserR "profile")) -> Server ProfileApi
profileServer carrier uid = pure $ carrier $ Out <$> send (GetProfile uid)
