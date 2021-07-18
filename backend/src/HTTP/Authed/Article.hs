{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Authed.Article (ArticleApi, articleServer) where

import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import HTTP.Util (CreateApi, DeleteApi, QP, RUDApi, ReadManyApi, ToggleApi)
import Servant (Capture, ServerT, type (:<|>) ((:<|>)), type (:>))

type CommentApi =
  Capture "id" (CommentR "id") :> DeleteApi CommentR "withAuthorProfile"
    :<|> CreateApi CommentR "withAuthorProfile"

type FavoriteApi = ToggleApi ArticleR "withAuthorProfile"

type ArticleApi =
  CreateApi ArticleR "withAuthorProfile"
    :<|> "feed" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleR "withAuthorProfile"
    :<|> ( Capture "slug" (ArticleR "id")
             :> ( RUDApi ArticleR "withAuthorProfile"
                    :<|> "comments" :> CommentApi
                    :<|> "favorite" :> FavoriteApi
                )
         )

-- FIXME
mkCommentServer :: ArticleR "id" -> ServerT CommentApi m
mkCommentServer = undefined

-- FIXME
mkFavoriteServer :: ArticleR "id" -> ServerT FavoriteApi m
mkFavoriteServer = undefined

-- FIXME
articleServer :: ServerT ArticleApi m
articleServer =
  undefined :<|> undefined
    :<|> (\aid -> (undefined :<|> undefined :<|> undefined) :<|> mkCommentServer aid :<|> mkFavoriteServer aid)
