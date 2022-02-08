-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect of visitors' action
--
-- @since 0.4.0.0
module Effect.VisitorAction where

import Data.Field.Tag (Tag)

-- | @since 0.3.0.0
-- Actions that can be carried out by visitor(__unauthenticated__).
data VisitorActionE f (m :: Type -> Type) a where
  -- | @since 0.3.0.0
  -- Get all the tags.
  GetTags :: VisitorActionE f m (f Tag)
