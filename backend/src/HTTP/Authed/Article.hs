{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Authed.Article (ArticleApi, articleServer) where

import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import HTTP.Util (CreateApi, DeleteApi, QP, RUDApi, ReadManyApi, ToggleApi)
import Servant (Capture, Server, type (:<|>) ((:<|>)), type (:>))

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
mkCommentServer :: ArticleR "id" -> Server CommentApi
mkCommentServer = undefined

-- FIXME
mkFavoriteServer :: ArticleR "id" -> Server FavoriteApi
mkFavoriteServer = undefined

-- FIXME
articleServer :: Server ArticleApi
articleServer =
  undefined :<|> undefined
    :<|> (\aid -> (undefined :<|> undefined :<|> undefined) :<|> mkCommentServer aid :<|> mkFavoriteServer aid)
