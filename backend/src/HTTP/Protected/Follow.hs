{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Protected.Follow (FollowApi, followServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.User (UserR (..))
import Domain.Util.Error (ValidationErr)
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (Cap, ToggleApi)
import Servant (ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified UserAction
import Validation (Validation (Failure, Success))

type FollowApi = Cap "username" (UserR "id") :> "follow" :> ToggleApi UserR "profile"

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
