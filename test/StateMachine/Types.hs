{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module StateMachine.Types where

import Data.Functor.Classes (Show1)
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR)
import Domain.Util.Field (Tag)
import GHC.Generics (Generic1)
import Orphans ()
import Test.StateMachine (Concrete, Reference, ToExpr, CommandNames, cmdName, cmdNames)
import qualified Test.StateMachine.Types.Rank2 as R2
import Text.Show (showString, showsPrec)

data VisitorCommand r
  = GetProfile (Reference (UserR "id") r)
  | GetArticle (Reference (ArticleR "id") r)
  | ListArticles
  | GetTags
  | GetComments (Reference (ArticleR "id") r)
  deriving (Show)

instance CommandNames VisitorCommand

data VisitorResponse (r :: Type -> Type)
  = GotProfile
  | GotArticle
  | ListedArticles
  | GotTags
  | GotComments
  deriving (Show)

data AuthCommand (r :: Type -> Type)
  = Register (UserR "create")
  | Login (Reference (UserR "login") r)
  | Logout
  deriving (Show)

instance CommandNames AuthCommand

data AuthResponse (r :: Type -> Type)
  = Registered (Reference (UserR "id") r) (Reference (UserR "login") r)
  | LoggedIn (Reference (UserR "token") r)
  | LoggedOut
  deriving (Show)

data UserCommand r
  = GetCurrentUser
  | UpdateUser (UserR "update")
  | FollowUser (Reference (UserR "id") r)
  | UnfollowUser (Reference (UserR "id") r)
  | CreateArticle (ArticleR "create")
  | UpdateArticle (Reference (ArticleR "id") r) (ArticleR "update")
  | DeleteArticle (Reference (ArticleR "id") r)
  | AddCommentToArticle (Reference (ArticleR "id") r) (CommentR "create")
  | DeleteComment (Reference (ArticleR "id") r) (Reference (CommentR "id") r)
  | FavoriteArticle (Reference (ArticleR "id") r)
  | UnfavoriteArticle (Reference (ArticleR "id") r)
  | FeedArticles
  deriving (Show)

instance CommandNames UserCommand

data UserResponse (r :: Type -> Type)
  = GotCurrentUser
  | UpdatedUser
  | FollowedUser
  | UnfollowedUser
  | CreatedArticle (Reference (ArticleR "id") r)
  | UpdatedArticle
  | DeletedArticle
  | AddedCommentToArticle (Reference (CommentR "id") r)
  | DeletedComment
  | FavoritedArticle
  | UnfavoritedArticle
  | FeededArticles
  deriving (Show)

data Command r
  = AuthCommand (Maybe (Reference (UserR "token") r)) (AuthCommand r)
  | VisitorCommand (Maybe (Reference (UserR "token") r)) (VisitorCommand r)
  | UserCommand (Maybe (Reference (UserR "token") r)) (UserCommand r)
  deriving (Show)

instance CommandNames Command where
  cmdName
    = \case
        (AuthCommand _ ac) -> cmdName ac
        (VisitorCommand _ vc) -> cmdName vc
        (UserCommand _ uc) -> cmdName uc
  cmdNames _ = cmdNames (Proxy @(AuthCommand _))
    <> cmdNames (Proxy @(VisitorCommand _))
    <> cmdNames (Proxy @(UserCommand _))

deriving instance Generic1 AuthCommand

instance R2.Functor AuthCommand

instance R2.Foldable AuthCommand

instance R2.Traversable AuthCommand

deriving instance Generic1 VisitorCommand

instance R2.Functor VisitorCommand

instance R2.Foldable VisitorCommand

instance R2.Traversable VisitorCommand

deriving instance Generic1 UserCommand

instance R2.Functor UserCommand

instance R2.Foldable UserCommand

instance R2.Traversable UserCommand

deriving instance Generic1 Command

instance R2.Functor Command

instance R2.Foldable Command

instance R2.Traversable Command

data Response r
  = AuthResponse (AuthResponse r)
  | VisitorResponse (VisitorResponse r)
  | UserResponse (UserResponse r)
  | FailResponse Text

instance Show1 r => Show (Response r) where
  showsPrec n =
    \case
      AuthResponse ar -> showsPrec n ar
      VisitorResponse vr -> showsPrec n vr
      UserResponse ur -> showsPrec n ur
      FailResponse txt -> showString $ toString txt

deriving instance Generic1 AuthResponse

instance R2.Functor AuthResponse

instance R2.Foldable AuthResponse

deriving instance Generic1 VisitorResponse

instance R2.Functor VisitorResponse

instance R2.Foldable VisitorResponse

deriving instance Generic1 UserResponse

instance R2.Functor UserResponse

instance R2.Foldable UserResponse

deriving instance Generic1 Response

instance R2.Functor Response

instance R2.Foldable Response

data Model r = Model
  { users :: [(Reference (UserR "id") r, UserR "create")],
    articles :: [(Reference (ArticleR "id") r, ArticleR "create")],
    logins :: [(Reference (UserR "id") r, Reference (UserR "login") r)],
    comments :: [(Reference (CommentR "id") r, CommentR "create")],
    userFollowUser :: Set (Reference (UserR "id") r, Reference (UserR "id") r),
    userFavoriteArticle :: Set (Reference (UserR "id") r, Reference (ArticleR "id") r),
    articleTaggedByTag :: Set (Reference (UserR "id") r, Tag),
    articleHasComment :: Set (Reference (ArticleR "id") r, Reference (CommentR "id") r),
    userCreateComment :: Set (Reference (UserR "id") r, Reference (CommentR "id") r),
    userCreateArticle :: Set (Reference (UserR "id") r, Reference (ArticleR "id") r),
    tokens :: [(Reference (UserR "token") r, Reference (UserR "login") r)]
  }
  deriving (Show, Eq, Generic)

instance ToExpr (Model Concrete)
