{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) fanshi1028 , 2021
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
import Domain.User (UserR)
import Domain.Util.Error (ValidationErr)
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (Cap, ReadApi)
import Servant (ServerT, type (:>))
import Validation (Validation (Failure, Success))
import VisitorAction (E (GetProfile))

-- * API

-- | @since 0.1.0.0
type ProfileApi = Cap "username" (UserR "id") :> ReadApi UserR "profile"

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
