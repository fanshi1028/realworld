-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect to get the current value
--
-- @since 0.1.0.0
module Current (E (..)) where

-- | @since 0.1.0.0
data E (e :: Type) (m :: Type -> Type) a where
  -- | @since 0.1.0.0
  -- get the current value
  GetCurrent :: E e m e
