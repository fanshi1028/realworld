{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | @since 0.4.0.0
module Client where

import API (Api)
import Client.Orphans ()
import Servant.API (type (:<|>) ((:<|>)))
import Servant.API.Flatten (flatten)
import Servant.Client.Streaming (client)

-- | @since 0.4.0.0
-- unflatten api client
(apisClient :<|> _ :<|> _) :<|> _ = client $ Proxy @Api

-- | @since 0.4.0.0
-- flatten api client
getUserClient
  :<|> getArticlesClient
  :<|> getArticlesStreamClient
  :<|> getArticleClient
  :<|> getCommentsClient
  :<|> getCommentsStreamClient
  :<|> getCurrentUserClient
  :<|> updateUserClient
  :<|> followUserClient
  :<|> unfollowUserClient
  :<|> getFeedsClient
  :<|> getFeedsStreamClient
  :<|> createArticleClient
  :<|> updateArticleClient
  :<|> deleteArticleClient
  :<|> deleteCommentClient
  :<|> createCommentClient
  :<|> favouriteArticleClient
  :<|> unfavouriteArticleClient
  :<|> loginClient
  :<|> registerClient
  :<|> getTagsClient
  :<|> getTagsStreamClient
  :<|> healthcheckClient = client . flatten $ Proxy @Api
