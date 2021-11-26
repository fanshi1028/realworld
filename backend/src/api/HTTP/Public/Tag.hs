{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server to read all tags
--
-- @since 0.1.0.0
module HTTP.Public.Tag where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Field.Tag (Tag)
import HTTP.Util (ReadManyApi)
import Servant (ServerT, type (:<|>) ((:<|>)))
import Servant.Types.SourceT (source)
import Util.JSON.To (Out (Out))
import VisitorAction (VisitorActionE (GetTags))

-- * API

-- | @since 0.3.0.0
type TagApi = ReadManyApi Tag

-- * Server

-- | @since 0.3.0.0
tagServer :: (Member (VisitorActionE []) sig, Algebra sig m) => ServerT TagApi m
tagServer =
  let tags = send GetTags
   in Out <$> tags :<|> (source <$> tags)
