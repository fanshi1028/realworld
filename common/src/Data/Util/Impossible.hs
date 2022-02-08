-- |
-- Description : Util
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Error for impossible
--
-- @since 0.4.0.0
module Data.Util.Impossible where

import Text.Show (showString, showsPrec)

-- | @since 0.4.0.0
newtype Impossible = Impossible Text

-- | @since 0.4.0.0
instance Show Impossible where
  showsPrec _ (Impossible err) = ("Impossible: " <>) . showString (toString err)
-- ^ >>> show $ Impossible "impossible"
--  "Impossible: impossible"
