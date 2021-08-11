{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
module HTTP.Authed.Article (ArticleApi, articleServer) where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.Util.Error (ValidationErr)
import Domain.Util.JSON.From (In (In))
import Domain.Util.JSON.To (Out (Out))
import HTTP.Util (CreateApi, QP, ReadManyApi, ToggleApi, UDApi)
import Servant (Capture, Delete, JSON, NoContent (NoContent), ServerT, type (:<|>) ((:<|>)), type (:>))
import qualified UserAction (E (AddCommentToArticle, CreateArticle, DeleteArticle, DeleteComment, FavoriteArticle, FeedArticles, UnfavoriteArticle, UpdateArticle))
import Validation (Validation (Failure, Success))

type CommentApi =
  Capture "id" (CommentR "id") :> Delete '[JSON] NoContent
    :<|> CreateApi CommentR "withAuthorProfile"

type FavoriteApi = ToggleApi ArticleR "withAuthorProfile"

type ArticleApi =
  CreateApi ArticleR "withAuthorProfile"
    :<|> "feed" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleR "withAuthorProfile"
    :<|> ( Capture "slug" (ArticleR "id")
             :> ( UDApi ArticleR "withAuthorProfile"
                    :<|> "comments" :> CommentApi
                    :<|> "favorite" :> FavoriteApi
                )
         )

articleServer ::
  ( Algebra sig m,
    Member UserAction.E sig,
    Member (Throw ValidationErr) sig
  ) =>
  ServerT ArticleApi m
articleServer =
  let fromUnValidatedInput f = \case
        In (Failure err) -> throwError err
        In (Success r) -> f r
   in fromUnValidatedInput (Out <<$>> send . UserAction.CreateArticle)
        -- FIXME
        :<|> (\_ _ -> Out <$> send UserAction.FeedArticles)
        :<|> ( \aid ->
                 ( (\(In au) -> Out <$> send (UserAction.UpdateArticle aid au))
                     :<|> send (UserAction.DeleteArticle aid) $> NoContent
                 )
                   -- Comment
                   :<|> ( (($> NoContent) . send . UserAction.DeleteComment aid)
                            :<|> fromUnValidatedInput (Out <<$>> send . UserAction.AddCommentToArticle aid)
                        )
                   -- Favourite
                   :<|> ( (Out <$> send (UserAction.FavoriteArticle aid))
                            :<|> (Out <$> send (UserAction.UnfavoriteArticle aid))
                        )
             )
