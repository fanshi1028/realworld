{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect and Carrier of visitors' action
--
-- @since 0.3.0.0
module VisitorAction where

import Field.Tag (Tag)

-- | @since 0.3.0.0
-- Actions that can be carried out by visitor(__unauthenticated__).
data VisitorActionE (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Get all the tags.
  GetTags :: VisitorActionE m [Tag]

