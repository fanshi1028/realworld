{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server for toggle user's following
--
-- @since 0.1.0.0
module HTTP.Protected.Follow where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import User (UserR (..))
import Util.Error (ValidationErr)
import Util.JSON.To (Out (Out))
import HTTP.Util (Cap, ToggleApi)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified UserAction
import Validation (Validation (Failure, Success))

-- * API

-- | @since 0.1.0.0
type FollowApi = Cap "username" (UserR "id") :> "follow" :> ToggleApi UserR "profile"

-- * Server

-- | @since 0.1.0.0
followServer ::
  ( Algebra sig m,
    Member UserAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT FollowApi m
followServer (Success uid) =
  Out <$> send (UserAction.FollowUser uid)
    :<|> (Out <$> send (UserAction.UnfollowUser uid))
followServer (Failure err) = throwError err :<|> throwError err
