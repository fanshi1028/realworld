-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect for invalidate JWT token
--
-- @since 0.1.0.0
module Token.JWT.Invalidate (E (..)) where

-- | @since 0.1.0.0
data E token (m :: Type -> Type) a where
  -- | Invalidate JWT token
  Invalidate :: token -> E token m ()
