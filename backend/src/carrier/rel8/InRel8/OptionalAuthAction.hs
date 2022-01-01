{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Description : Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Carrier of optionally authed action with postgres storage
--
-- @since 0.4.0.0
module InRel8.OptionalAuthAction where

import Authentication (AuthenticationE (GetCurrentAuth))
import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Catch (Catch)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain (Domain (Article, Comment, User))
import InRel8.Sql (SqlInRel8E (SqlSelect))
import InRel8.Storage (getArticles, getProfile, getUserById, mkArticle, mkProfile)
import qualified InRel8.Storage.Schema.Article as Article (slug)
import OptionalAuthAction (OptionalAuthActionE (GetArticle, GetProfile))
import Rel8 (filter, lit, select, (==.))
import Storage.Error (NotFound (NotFound))
import Storage.Map (IdNotFound, IdOf)

-- | @since 0.4.0.0
newtype OptionalAuthActionInRel8C m a = OptionalAuthActionInRel8C
  { -- | @since 0.4.0.0
    runOptionalAuthActionInRel8 :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.4.0.0
instance
  ( Member (Throw Text) sig,
    Member (Catch (IdNotFound 'Comment)) sig,
    Member (AuthenticationE 'User) sig,
    Member SqlInRel8E sig,
    Member (Throw (NotFound (IdOf 'User))) sig,
    Member (Throw (NotFound (IdOf 'Article))) sig,
    Algebra sig m
  ) =>
  Algebra (OptionalAuthActionE :+: sig) (OptionalAuthActionInRel8C m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> do
      mAuth <- send $ GetCurrentAuth @'User
      case action of
        GetProfile uid -> do
          send (SqlSelect . select $ getUserById (lit uid) >>= getProfile mAuth) >>= \case
            [] -> throwError $ NotFound uid
            [p] -> pure $ mkProfile p
            _ -> throwError @Text $ "impossible: got more than one profile for user id: " <> show uid
        GetArticle aid -> do
          send
            ( SqlSelect . select $
                getArticles mAuth
                  >>= Rel8.filter (\(Article.slug -> aid', _, _, _, _, _) -> lit aid ==. aid')
            )
            >>= \case
              [] -> throwError $ NotFound aid
              [a] -> pure $ mkArticle a
              _ -> throwError @Text $ "impossible: got more than one article for article id: " <> show aid
  alg hdl (R other) ctx = OptionalAuthActionInRel8C $ alg (runOptionalAuthActionInRel8 . hdl) other ctx
  {-# INLINE alg #-}