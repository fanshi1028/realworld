{-# LANGUAGE DataKinds #-}

-- |
module HTTP.Public.Article (ArticleApi, articleServer) where

import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import HTTP.Util (QP, ReadManyApi)
import Servant (Capture, Server, type (:<|>), type (:>))

type ArticleApi =
  Capture "slug" (ArticleR "id")
    :> ( QP "tag" :> QP "author" :> QP "favorited" :> QP "limit" :> QP "offset" :> ReadManyApi ArticleR "withAuthorProfile"
           :<|> "comments" :> ReadManyApi CommentR "withAuthorProfile"
       )

-- FIXME
articleServer :: Server ArticleApi
articleServer = undefined
