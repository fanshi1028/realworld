{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier of optionally authed action(many) with postgres storage
--
-- @since 0.4.0.0
module InRel8.OptionalAuthAction.Many where

import Authentication (AuthenticationE (GetCurrentAuth))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Data.Domain (Domain (User))
import Data.Storage.Map (IdOf (..))
import InRel8.Sql (SqlInRel8E (SqlSelect))
import InRel8.Storage (getArticles, getAuthorForComment, getTagForArticle, getUserFavoritingForArticle, isFollow, mkArticle, mkComment)
import InRel8.Storage.Schema.Comment (commentSchema)
import qualified InRel8.Storage.Schema.Comment as CommentRel8 (article)
import InRel8.Storage.Schema.Tag (tag)
import InRel8.Storage.Schema.User as UserRel8 (UserRel8 (..))
import OptionalAuthAction.Many (OptionalAuthActionManyE (GetComments, ListArticles))
import Rel8 (each, filter, lit, present, select, where_, (==.))

-- | @since 0.4.0.0
newtype OptionalAuthActionManyInRel8C (f :: Type -> Type) m a = OptionalAuthActionManyInRel8C
  { -- | @since 0.4.0.0
    runOptionalAuthActionManyInRel8 :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.4.0.0
instance
  ( Algebra sig m,
    Member (AuthenticationE 'User) sig,
    Member SqlInRel8E sig
  ) =>
  Algebra
    ( OptionalAuthActionManyE [] :+: sig
    )
    (OptionalAuthActionManyInRel8C [] m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      mAuth <- send $ GetCurrentAuth @'User
      send . SqlSelect $ case action of
        ListArticles mTag mAuthor mFav ->
          mkArticle <<$>> do
            select $ do
              r@(a, u, _, _, _, _) <- getArticles mAuth
              let ifJust ma f = maybe (pure ()) f ma
              ifJust mAuthor $ \u' ->
                where_ $ lit (UserId u') ==. UserRel8.username u
              ifJust mFav $ \(lit . UserId -> u') ->
                present $ getUserFavoritingForArticle a >>= Rel8.filter ((u' ==.) . UserRel8.username)
              ifJust mTag $ \(lit -> t) ->
                present $ getTagForArticle a >>= Rel8.filter ((t ==.) . tag)
              pure r
        GetComments (lit -> aid) ->
          mkComment <<$>> do
            select $ do
              cm <- each commentSchema >>= Rel8.filter ((==. aid) . CommentRel8.article)
              u <- getAuthorForComment cm
              (cm,u,) <$> isFollow mAuth u
  alg hdl (R other) ctx = OptionalAuthActionManyInRel8C $ alg (runOptionalAuthActionManyInRel8 . hdl) other ctx
  {-# INLINE alg #-}
