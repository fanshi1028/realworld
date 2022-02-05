{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Typeclass Export
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- typeclass eport for HasToken
--
-- @since 0.4.0.0
module Data.Token.HasToken
  ( -- * Typeclass

    -- ** HasToken
    module X,
  )
where

import Data.Token.Internal.HasToken as X
import Data.Token.Internal.HasToken.User as X
