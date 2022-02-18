{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Description : Server
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Auth protected Server
--
-- @since 0.1.0.0
module Server.Protected where

import API.Protected (ProtectedApi, ProtectedArticleApi, ProtectedFollowApi, ProtectedUserApi)
import Control.Algebra (Algebra, send)
import qualified Control.Effect.Reader as R (Reader, ask)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Data.Domain (Domain (User))
import Data.Domain.User (UserAuthWithToken (UserAuthWithToken))
import Data.Paging (HasPaging (paging), Limit, Offset, Paging (LimitOffset))
import Data.Util.JSON.From (In (In))
import Data.Util.JSON.To (Out (Out))
import Data.Util.Validation (ValidationErr)
import Token.Create (CreateTokenE (CreateToken))
import UserAction (UserActionE (AddCommentToArticle,
                                CreateArticle, DeleteArticle, DeleteComment, FavoriteArticle, FollowUser, GetCurrentUser, UnfavoriteArticle, UnfollowUser, UpdateArticle, UpdateUser))
import UserAction.Many (UserActionManyE (FeedArticles))
import Servant (NoContent (NoContent), ServerT, type (:<|>) ((:<|>)))
import Servant.Types.SourceT (source)
import Validation (Validation (Failure, Success), validation)

-- * Server

-- | @since 0.2.0.0
userServer ::
  ( Algebra sig m,
    Member (Throw ValidationErr) sig,
    Member (CreateTokenE 'User) sig,
    Member UserActionE sig
  ) =>
  ServerT ProtectedUserApi m
userServer =
  Out <$> send GetCurrentUser
    :<|> ( \case
             In (Failure err) -> throwError err
             In (Success a) -> do
               newUser <- send (UpdateUser a)
               Out . UserAuthWithToken newUser <$> send (CreateToken newUser)
         )

-- | @since 0.1.0.0
followServer ::
  ( Algebra sig m,
    Member UserActionE sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ProtectedFollowApi m
followServer (Success uid) =
  Out <$> send (FollowUser uid)
    :<|> (Out <$> send (UnfollowUser uid))
followServer (Failure err) = throwError err :<|> throwError err

-- | @since 0.3.0.0
articleServer ::
  ( Algebra sig m,
    Member UserActionE sig,
    Member (UserActionManyE []) sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ProtectedArticleApi m
articleServer =
  let fromUnValidatedInput f = \case
        In (Failure err) -> throwError err
        In (Success r) -> f r
   in ( \mLimit mOffset -> do
          let getVPaging =
                liftA2 LimitOffset
                  <$> (R.ask <&> \lim -> fromMaybe (pure lim) mLimit)
                  <*> (R.ask <&> \off -> fromMaybe (pure off) mOffset)
              fa =
                getVPaging
                  >>= validation
                    (throwError @ValidationErr)
                    (\p -> paging p <$> send FeedArticles)
           in Out <$> fa :<|> (source <$> fa)
      )
        :<|> fromUnValidatedInput (Out <<$>> send . CreateArticle)
        :<|> ( \case
                 Success aid ->
                   ( fromUnValidatedInput (Out <<$>> send . UpdateArticle aid)
                       :<|> send (DeleteArticle aid) $> NoContent
                   )
                     -- Comment
                     :<|> ( \case
                              Success cid -> send (DeleteComment aid cid) $> NoContent
                              Failure err -> throwError err
                              :<|> fromUnValidatedInput (Out <<$>> send . AddCommentToArticle aid)
                          )
                     -- Favourite
                     :<|> ( (Out <$> send (FavoriteArticle aid))
                              :<|> (Out <$> send (UnfavoriteArticle aid))
                          )
                 Failure err ->
                   (const (throwError err) :<|> throwError err)
                     :<|> ( (const (throwError err) :<|> const (throwError err))
                              :<|> (throwError err :<|> throwError err)
                          )
             )

-- | @since 0.3.0.0
authedServer ::
  ( Algebra sig m,
    Member UserActionE sig,
    Member (R.Reader Limit) sig,
    Member (R.Reader Offset) sig,
    Member (UserActionManyE []) sig,
    Member (Throw ValidationErr) sig,
    Member (CreateTokenE 'User) sig
  ) =>
  ServerT ProtectedApi m
authedServer = userServer :<|> followServer :<|> articleServer
