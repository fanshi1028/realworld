{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server to read user's profile
--
-- @since 0.1.0.0
module HTTP.Public.Profile where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import HTTP.Util (Cap, ReadApi)
import Servant (ServerT, type (:>))
import Storage.Map (IdOf)
import User (UserR)
import Util.Error (ValidationErr)
import Util.JSON.To (Out (Out))
import Validation (Validation (Failure, Success))
import VisitorAction (E (GetProfile))

-- * API

-- | @since 0.1.0.0
type ProfileApi = Cap "username" (IdOf "user") :> ReadApi "user" (UserR "profile")

-- * Server

-- | @since 0.1.0.0
profileServer ::
  ( Algebra sig m,
    Member VisitorAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ProfileApi m
profileServer (Success u) = Out <$> send (GetProfile u)
profileServer (Failure err) = throwError err
