{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module StateMachine.Types where

import Data.Functor.Classes (Show1)
import Field.Email (Email)
import Field.Password (Password (Password))
import GHC.Generics (Generic1)
import Orphans ()
import Storage.Map (CreateOf (..), IdOf (..), UpdateOf (..), Patch)
import Test.StateMachine (CommandNames, Concrete, Reference, ToExpr, cmdName, cmdNames)
import qualified Test.StateMachine.Types.Rank2 as R2
import Text.Show (showString, showsPrec)
import Token (TokenOf (..))

data VisitorCommand r
  = GetProfile (Reference (IdOf "user") r)
  | GetArticle (Reference (IdOf "article") r)
  | ListArticles
  | GetTags
  | GetComments (Reference (IdOf "article") r)
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
  = Register (CreateOf "user")
  | Login (Reference Email r) (Reference Password r)
  | Logout
  deriving (Show)

instance CommandNames AuthCommand

data AuthResponse (r :: Type -> Type)
  = Registered (Reference (IdOf "user") r) (Reference Email r) (Reference Password r) (Reference (TokenOf "user") r)
  | LoggedIn
  | LoggedOut
  deriving (Show)

data UserCommand r
  = GetCurrentUser
  | UpdateUser (Patch (UpdateOf "user"))
  | FollowUser (Reference (IdOf "user") r)
  | UnfollowUser (Reference (IdOf "user") r)
  | CreateArticle (CreateOf "article")
  | UpdateArticle (Reference (IdOf "article") r) (Patch (UpdateOf "article"))
  | DeleteArticle (Reference (IdOf "article") r)
  | AddCommentToArticle (Reference (IdOf "article") r) (CreateOf "comment")
  | DeleteComment (Reference (IdOf "article") r) (Reference (IdOf "comment") r)
  | FavoriteArticle (Reference (IdOf "article") r)
  | UnfavoriteArticle (Reference (IdOf "article") r)
  | FeedArticles
  deriving (Show)

instance CommandNames UserCommand

data UserResponse (r :: Type -> Type)
  = GotCurrentUser
  | UpdatedUser (Reference (TokenOf "user") r) (Maybe (Reference (IdOf "user") r)) (Maybe (Reference Email r)) (Maybe (Reference Password r))
  | FollowedUser
  | UnfollowedUser
  | CreatedArticle (Reference (IdOf "article") r)
  | UpdatedArticle (Maybe (Reference (IdOf "article") r))
  | DeletedArticle
  | AddedCommentToArticle (Reference (IdOf "comment") r)
  | DeletedComment
  | FavoritedArticle
  | UnfavoritedArticle
  | FeededArticles
  deriving (Show)

data Command r
  = AuthCommand (Maybe (Reference (TokenOf "user") r)) (AuthCommand r)
  | VisitorCommand (Maybe (Reference (TokenOf "user") r)) (VisitorCommand r)
  | UserCommand (Maybe (Reference (TokenOf "user") r)) (UserCommand r)
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
  { users :: [(Reference (IdOf "user") r, CreateOf "user")],
    articles :: [(Reference (IdOf "article") r, CreateOf "article")],
    credentials :: [(Reference Email r, Reference Password r)],
    emails :: [(Reference (IdOf "user") r, Reference Email r)],
    comments :: [(Reference (IdOf "comment") r, CreateOf "comment")],
    userFollowUser :: Set (Reference (IdOf "user") r, Reference (IdOf "user") r),
    userFavoriteArticle :: Set (Reference (IdOf "user") r, Reference (IdOf "article") r),
    articleHasComment :: Set (Reference (IdOf "article") r, Reference (IdOf "comment") r),
    userCreateComment :: Set (Reference (IdOf "user") r, Reference (IdOf "comment") r),
    userCreateArticle :: Set (Reference (IdOf "user") r, Reference (IdOf "article") r),
    tokens :: [(Reference (TokenOf "user") r, Reference Email r)]
  }
  deriving (Show, Eq, Generic)

instance ToExpr (Model Concrete)
