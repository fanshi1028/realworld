{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.Tag (TagApi, tagServer, TagServerEffect) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Domain.Util.Field as F (Tag)
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (EffRunner)
import Servant (Get, JSON, Server)
import Tag.Effect as E (Tag (GetTags))

type TagApi = Get '[JSON] (Out [F.Tag])

type TagServerEffect sig m = (Member (E.Tag []) sig, Algebra sig m)

tagServer ::
  (TagServerEffect sig m) =>
  EffRunner m (Out [F.Tag]) ->
  Server TagApi
tagServer carrier = pure $ carrier $ Out <$> send GetTags
