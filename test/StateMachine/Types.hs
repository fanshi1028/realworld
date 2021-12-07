{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module StateMachine.Types where

import Data.Functor.Classes (Show1)
import Domain (Domain (Article, Comment, User))
import Field.Email (Email)
import Field.Password (Password)
import GHC.Generics (Generic1)
import Orphans ()
import Storage.Map (CreateOf (..), IdOf (..), Patch, UpdateOf (..))
import Test.StateMachine (CommandNames, Concrete, Reference, ToExpr, cmdName, cmdNames)
import qualified Test.StateMachine.Types.Rank2 as R2
import Text.Show (showString, showsPrec)
import Token.HasToken (TokenOf (..))

data VisitorCommand r
  = GetProfile (Reference (IdOf 'User) r)
  | GetArticle (Reference (IdOf 'Article) r)
  | ListArticles
  | GetTags
  | GetComments (Reference (IdOf 'Article) r)
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
  = Register (CreateOf 'User)
  | Login (Reference Email r) (Reference Password r)
  -- -- | Logout
  deriving (Show)

instance CommandNames AuthCommand

data AuthResponse (r :: Type -> Type)
  = Registered (Reference (IdOf 'User) r) (Reference Email r) (Reference Password r) (Reference (TokenOf 'User) r)
  | LoggedIn
  -- -- | LoggedOut
  deriving (Show)

data UserCommand r
  = GetCurrentUser
  | UpdateUser (Patch (UpdateOf 'User))
  | FollowUser (Reference (IdOf 'User) r)
  | UnfollowUser (Reference (IdOf 'User) r)
  | CreateArticle (CreateOf 'Article)
  | UpdateArticle (Reference (IdOf 'Article) r) (Patch (UpdateOf 'Article))
  | DeleteArticle (Reference (IdOf 'Article) r)
  | AddCommentToArticle (Reference (IdOf 'Article) r) (CreateOf 'Comment)
  | DeleteComment (Reference (IdOf 'Article) r) (Reference (IdOf 'Comment) r)
  | FavoriteArticle (Reference (IdOf 'Article) r)
  | UnfavoriteArticle (Reference (IdOf 'Article) r)
  | FeedArticles
  deriving (Show)

instance CommandNames UserCommand

data UserResponse (r :: Type -> Type)
  = GotCurrentUser
  | UpdatedUser (Reference (TokenOf 'User) r) (Maybe (Reference (IdOf 'User) r)) (Maybe (Reference Email r)) (Maybe (Reference Password r))
  | FollowedUser
  | UnfollowedUser
  | CreatedArticle (Reference (IdOf 'Article) r)
  | UpdatedArticle (Maybe (Reference (IdOf 'Article) r))
  | DeletedArticle
  | AddedCommentToArticle (Reference (IdOf 'Comment) r)
  | DeletedComment
  | FavoritedArticle
  | UnfavoritedArticle
  | FeededArticles
  deriving (Show)

data Command r
  = AuthCommand (Maybe (Reference (TokenOf 'User) r)) (AuthCommand r)
  | VisitorCommand (Maybe (Reference (TokenOf 'User) r)) (VisitorCommand r)
  | UserCommand (Maybe (Reference (TokenOf 'User) r)) (UserCommand r)
  deriving (Show)

instance CommandNames Command where
  cmdName =
    \case
      (AuthCommand _ ac) -> cmdName ac
      (VisitorCommand _ vc) -> cmdName vc
      (UserCommand _ uc) -> cmdName uc
  cmdNames _ =
    cmdNames (Proxy @(AuthCommand _))
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
  { users :: [(Reference (IdOf 'User) r, CreateOf 'User)],
    articles :: [(Reference (IdOf 'Article) r, CreateOf 'Article)],
    credentials :: [(Reference Email r, Reference Password r)],
    emails :: [(Reference (IdOf 'User) r, Reference Email r)],
    comments :: [(Reference (IdOf 'Comment) r, CreateOf 'Comment)],
    userFollowUser :: Set (Reference (IdOf 'User) r, Reference (IdOf 'User) r),
    userFavoriteArticle :: Set (Reference (IdOf 'User) r, Reference (IdOf 'Article) r),
    articleHasComment :: Set (Reference (IdOf 'Article) r, Reference (IdOf 'Comment) r),
    userCreateComment :: Set (Reference (IdOf 'User) r, Reference (IdOf 'Comment) r),
    userCreateArticle :: Set (Reference (IdOf 'User) r, Reference (IdOf 'Article) r),
    tokens :: [(Reference (TokenOf 'User) r, Reference Email r)]
  }
  deriving (Show, Eq, Generic)

instance ToExpr (Model Concrete)
