{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Public.Tag (TagApi, tagServer) where

import Domain.Util.Field (Tag)
import Domain.Util.JSON.To (Out)
import Servant (Get, JSON, Server)

type TagApi = Get '[JSON] (Out [Tag])

tagServer :: Server TagApi
tagServer = undefined
