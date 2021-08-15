{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.Profile (ProfileApi, profileServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.User (UserR)
import Domain.Util.Error (ValidationErr)
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (Cap, ReadApi)
import Servant (ServerT, type (:>))
import Validation (Validation (Failure, Success))
import VisitorAction (E (GetProfile))

type ProfileApi = Cap "username" (UserR "id") :> ReadApi UserR "profile"

profileServer ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ProfileApi m
profileServer (Success u) = Out <$> send (GetProfile u)
profileServer (Failure err) = throwError err
