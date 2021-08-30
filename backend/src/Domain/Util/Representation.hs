{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Transforms between different representations of Domain Types.
--
-- @since 0.1.0.0
module Domain.Util.Representation (Transform (transform)) where

import Control.Algebra (Algebra, send)
import Control.Effect.Catch (Catch, catchError)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Current (E (GetCurrent))
import qualified Data.Text as Text (intercalate, toLower)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists (AlreadyExists), NotAuthorized, NotFound)
import Domain.Util.Field (Bio (Bio), Email, Image (Image), Slug (Slug), Tag, Time, Title (..), Username)
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)
import qualified GenUUID (E (Generate))
import qualified Relation.ManyToMany (E (GetRelatedLeft, GetRelatedRight, IsRelated, Relate))
import qualified Relation.ToOne (E (GetRelated))
import Relude.Extra (un)
import qualified Storage.Map (E (GetById))
import Token (E (CreateToken))

-- | Transform between different representation of the same data
--
-- @since 0.1.0.0
class Transform (r :: Symbol -> Type) (s1 :: Symbol) (s2 :: Symbol) (m :: Type -> Type) where
  transform :: (Monad m) => r s1 -> m (r s2)

-- * User

-- | @since 0.1.0.0
instance (HasField "username" (UserR s) Username) => Transform UserR s "id" m where
  transform = pure . UserId . getField @"username"

-- | @since 0.1.0.0
--
-- FIXME pw hashing
instance
  ( Algebra sig m,
    Member (Current.E Time) sig,
    Member (Storage.Map.E UserR) sig,
    Member (Relation.ToOne.E Email "of" (UserR "id")) sig,
    Member (Throw (AlreadyExists (UserR "id"))) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Catch (NotFound (UserR "id"))) sig
  ) =>
  Transform UserR "create" "all" m
  where
  transform (UserRegister user em pw) = do
    void $ send $ GetCurrent @Time
    send (Relation.ToOne.GetRelated @_ @"of" @(UserR "id") em) >>= \case
      Just _ -> throwError $ AlreadyExists em
      Nothing ->
        ( send (Storage.Map.GetById $ UserId user)
            >> throwError (AlreadyExists $ UserId user)
        )
          `catchError` const @_ @(NotFound (UserR "id")) (pure $ User em pw user (Bio "") (Image ""))

-- | @since 0.1.0.0
instance Transform UserR "all" "auth" m where
  transform (User em _ name bio' img) = pure $ UserAuth em name bio' img

-- | @since 0.1.0.0
instance (Algebra sig m, Member (Token.E UserR) sig) => Transform UserR "auth" "authWithToken" m where
  transform auth = UserAuthWithToken auth <$> send (CreateToken auth)

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Transform UserR "all" "auth" m,
    Transform UserR "auth" "authWithToken" m
  ) =>
  Transform UserR "all" "authWithToken" m
  where
  transform = transform >=> (transform @_ @"auth")

instance
  ( Algebra sig m,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Catch (NotAuthorized UserR)) sig,
    Transform UserR "auth" "id" m
  ) =>
  Transform UserR "auth" "profile" m
  where
  transform auth =
    UserProfile auth
      <$> ( do
              cUserId <-
                send (GetCurrent @(UserR "authWithToken"))
                  >>= \(UserAuthWithToken auth' _) -> transform auth'
              transform auth
                >>= send . Relation.ManyToMany.IsRelated @(_ "id") @(_ "id") @"follow" cUserId
          )
        `catchError` const @_ @(NotAuthorized UserR) (pure False)

-- | @since 0.1.0.0
instance (Algebra sig m, Transform UserR "auth" "profile" m, Transform UserR "all" "auth" m) => Transform UserR "all" "profile" m where
  transform = transform >=> transform @_ @"auth"

-- * Article

-- | @since 0.1.0.0
instance (HasField "title" (ArticleR s) Title) => Transform ArticleR s "id" m where
  transform = pure . ArticleId . Slug . Text.intercalate "-" . words . Text.toLower . un . getField @"title"

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Member (Current.E Time) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Relation.ManyToMany.E (ArticleR "id") "taggedBy" Tag) sig
  ) =>
  Transform ArticleR "create" "all" m
  where
  transform ac@(ArticleCreate tt des bd ts) = do
    t <- send $ GetCurrent @Time
    UserAuthWithToken auth _ <- send $ GetCurrent @(UserR "authWithToken")
    aid <- transform ac
    foldMapA (send . Relation.ManyToMany.Relate @(ArticleR "id") @_ @"taggedBy" aid) ts
    Article tt des bd t t <$> transform auth

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Member (Current.E (UserR "authWithToken")) sig,
    Member (Storage.Map.E UserR) sig,
    Member (Relation.ManyToMany.E (ArticleR "id") "taggedBy" Tag) sig,
    Member (Relation.ManyToMany.E (UserR "id") "favorite" (ArticleR "id")) sig,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Transform UserR "all" "profile" m
  ) =>
  Transform ArticleR "all" "withAuthorProfile" m
  where
  transform a = do
    UserAuthWithToken auth _ <- send $ GetCurrent @(UserR "authWithToken")
    aid <- transform a
    uid <- transform auth
    ArticleWithAuthorProfile aid a
      <$> send (Relation.ManyToMany.GetRelatedLeft @(ArticleR "id") @"taggedBy" @Tag aid)
      <*> send (Relation.ManyToMany.IsRelated @(UserR "id") @_ @"favorite" uid aid)
      <*> (fromIntegral . length <$> send (Relation.ManyToMany.GetRelatedRight @_ @(UserR "id") @"favorite" aid))
      <*> (getField @"author" a & send . Storage.Map.GetById @UserR >>= transform)

-- * Comment

-- | @since 0.1.0.0
instance {-# OVERLAPPABLE #-} (HasField "id" (CommentR s) (CommentR "id")) => Transform CommentR s "id" m where
  transform = pure . getField @"id"

-- | @since 0.1.0.0
instance {-# OVERLAPPING #-} (Algebra sig m, Member GenUUID.E sig) => Transform CommentR "create" "id" m where
  transform _ = CommentId <$> send GenUUID.Generate

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Member (Current.E Time) sig,
    Member (Current.E (UserR "authWithToken")) sig,
    Transform CommentR "create" "id" m,
    MonadReader (ArticleR "id") m
  ) =>
  Transform CommentR "create" "all" m
  where
  transform cc@(CommentCreate txt) = do
    t <- send $ Current.GetCurrent @Time
    Comment
      <$> transform cc
      <*> pure t
      <*> pure t
      <*> pure txt
      <*> ( send (Current.GetCurrent @(UserR "authWithToken"))
              >>= \(UserAuthWithToken auth _) -> transform auth
          )
      <*> ask @(ArticleR "id")

-- | @since 0.1.0.0
instance
  ( Algebra sig m,
    Member (Storage.Map.E UserR) sig,
    Transform UserR "all" "profile" m
  ) =>
  Transform CommentR "all" "withAuthorProfile" m
  where
  transform (Comment cid t1 t2 txt uid _) =
    send (Storage.Map.GetById uid)
      >>= transform
      <&> CommentWithAuthorProfile cid t1 t2 txt
