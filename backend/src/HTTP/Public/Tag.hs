{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.Tag (TagApi, tagServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Domain.Util.Field as F (Tag)
import Domain.Util.JSON.To (Out (Out))
import Servant (Get, JSON, ServerT)
import Tag.Effect as E (Tag (GetTags))

type TagApi = Get '[JSON] (Out [F.Tag])

tagServer :: (Member (E.Tag []) sig, Algebra sig m) => ServerT TagApi m
tagServer = Out <$> send GetTags
