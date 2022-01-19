{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Public API & Server, for visitors.
--
-- @since 0.1.0.0
module Server.Public where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import HTTP.Public (PublicApi, PublicTagApi)
import Servant (ServerT, type (:<|>) ((:<|>)))
import Servant.Types.SourceT (source)
import Util.JSON.To (Out (Out))
import VisitorAction (VisitorActionE (GetTags))

-- * Server

-- | @since 0.3.0.0
tagServer :: (Member (VisitorActionE []) sig, Algebra sig m) => ServerT PublicTagApi m
tagServer =
  let tags = send GetTags
   in Out <$> tags :<|> (source <$> tags)

-- | @since 0.3.0.0
publicServer ::
  ( Algebra sig m,
    Member (VisitorActionE []) sig
  ) =>
  ServerT PublicApi m
publicServer = tagServer
