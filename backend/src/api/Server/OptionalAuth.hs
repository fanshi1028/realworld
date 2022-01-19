{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : API & Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- API & Server, optional authed.
--
-- @since 0.3.0.0
module Server.OptionalAuth where

import Control.Algebra (Algebra, send)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import HTTP.OptionalAuth (OptionalAuthApi, OptionalAuthArticleApi, OptionalAuthProfileApi)
import OptionalAuthAction (OptionalAuthActionE (GetArticle, GetProfile))
import OptionalAuthAction.Many (OptionalAuthActionManyE (GetComments, ListArticles))
import Paging (HasPaging (paging), Limit, Offset, Paging (LimitOffset))
import Servant (ServerT, type (:<|>) ((:<|>)))
import Servant.Types.SourceT (source)
import Util.JSON.To (Out (Out))
import Util.Validation (ValidationErr)
import Validation (Validation (Failure, Success), validation)

-- * Server

-- | @since 0.3.0.0
profileServer ::
  ( Algebra sig m,
    Member OptionalAuthActionE sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT OptionalAuthProfileApi m
profileServer (Success u) = Out <$> send (GetProfile u)
profileServer (Failure err) = throwError err

-- | @since 0.3.0.0
articleServer ::
  ( Algebra sig m,
    Member OptionalAuthActionE sig,
    Member (OptionalAuthActionManyE []) sig,
    Member (Throw ValidationErr) sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig
  ) =>
  ServerT OptionalAuthArticleApi m
articleServer =
  ( \mTag mAuthor mFavBy mLimit mOffset -> do
      let getVPaging =
            liftA2 LimitOffset
              <$> (R.ask <&> \lim -> fromMaybe (pure lim) mLimit)
              <*> (R.ask <&> \off -> fromMaybe (pure off) mOffset)
          la =
            getVPaging >>= \vp ->
              validation
                (throwError @ValidationErr)
                (\(act, p) -> paging p <$> send act)
                (liftA2 (,) (ListArticles @[] <$> sequenceA mTag <*> sequenceA mAuthor <*> sequenceA mFavBy) vp)
       in Out <$> la :<|> (source <$> la)
  )
    :<|> ( \case
             Success aid ->
               Out <$> send (GetArticle aid)
                 :<|> ( let gc = send $ GetComments aid
                         in Out <$> gc :<|> (source <$> gc)
                      )
             Failure err -> throwError err :<|> throwError err :<|> throwError err
         )

-- | @since 0.3.0.0
optionallyAuthedServer ::
  ( Algebra sig m,
    Member OptionalAuthActionE sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig,
    Member (OptionalAuthActionManyE []) sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT OptionalAuthApi m
optionallyAuthedServer = profileServer :<|> articleServer
