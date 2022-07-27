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
module Data.Authentication.HasToken
  ( -- * Typeclass

    -- ** HasToken
    module X,
  )
where

import Data.Authentication.HasToken.Internal as X
import Data.Authentication.HasToken.Internal.User as X
