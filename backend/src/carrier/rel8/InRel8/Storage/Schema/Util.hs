{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Util
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- helper for schema field label
--
-- @since 0.4.0.0
module InRel8.Storage.Schema.Util (snakeNamesFromLabels) where

import qualified Data.List.NonEmpty as NE (intersperse)
import GHC.Unicode (isLower, isUpper, toLower)
import Rel8 (Name, Table, namesFromLabelsWith)

-- | @since 0.4.0.0
snakeNamesFromLabels :: Table Name a => a
snakeNamesFromLabels = namesFromLabelsWith $ \s ->
  fold $ NE.intersperse "/" $ camelToSnake <$> s

-- | @since 0.4.0.0
camelToSnake :: String -> String
camelToSnake (break isUpper -> (a, a')) =
  a <> case a' of
    [] -> ""
    (break isLower -> (fmap toLower -> b, b')) ->
      "_" <> case b' of
        [] -> b
        _ -> case nonEmpty b of
          Nothing -> error "impossible"
          Just (init &&& last -> (c, ch)) -> (<> camelToSnake (ch : b')) $ case c of
            [] -> ""
            _ -> c <> "_"
