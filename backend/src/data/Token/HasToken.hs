{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Typeclass Export
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- typeclass eport for HasToken
--
-- @since 0.3.0.0
module Token.HasToken
  ( -- * Typeclass

    -- ** HasToken
    module X,
  )
where

import Token.Internal.HasToken as X
import Token.Internal.HasToken.User as X
