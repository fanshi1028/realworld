{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier of user's action (many)
--
-- @since 0.4.0.0
module InRel8.UserAction.Many where

import Authentication (AuthenticationE (GetCurrentAuth))
import Control.Algebra (Algebra, alg, send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Data.Authentication.HasAuth (AuthOf (..))
import Data.Domain (Domain (User))
import Data.Storage.Map.HasStorage (toUserId)
import InRel8.Sql (SqlInRel8E (SqlSelect))
import InRel8.Storage (filterUserFollowUser, getArticles, mkArticle)
import InRel8.Storage.Schema.User as UserRel8 (username)
import Rel8 (lit, select)
import UserAction.Many (UserActionManyE (FeedArticles))

-- | @since 0.4.0.0
newtype UserActionManyInRel8C (f :: Type -> Type) m a = UserActionManyInRel8C
  { -- | @since 0.4.0.0
    runUserActionManyInRel8 :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.4.0.0
instance
  ( Member (AuthenticationE 'User) sig,
    Member SqlInRel8E sig,
    Algebra sig m
  ) =>
  Algebra (UserActionManyE [] :+: sig) (UserActionManyInRel8C [] m)
  where
  alg _ (L FeedArticles) ctx =
    (<$ ctx) <$> do
      send (GetCurrentAuth @'User) >>= \case
        Nothing -> error "impossible: not login would not get here"
        mAuth@(Just (lit . toUserId -> uid)) -> send . SqlSelect . (mkArticle <<$>>) . select $ do
          getArticles mAuth
            >>= \r@(_, UserRel8.username -> uid', _, _, _, _) ->
              filterUserFollowUser uid uid' $> r
  alg hdl (R other) ctx = UserActionManyInRel8C $ alg (runUserActionManyInRel8 . hdl) other ctx
  {-# INLINE alg #-}
