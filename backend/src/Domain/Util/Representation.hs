{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Domain.Util.Representation where

import Authentication.Token (E (CreateToken))
import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import Current (E (GetCurrent))
import Data.Generic.HKD (Construct (construct, deconstruct), HKD)
import qualified Data.Semigroup as SG (Last (getLast))
import qualified Data.Text as Text (intercalate, toLower)
import Domain.Article (ArticleR (..))
import Domain.Comment (CommentR (..))
import Domain.User (UserR (..))
import Domain.Util.Error (AlreadyExists (AlreadyExists))
import Domain.Util.Field (Bio (Bio), Email, Image (Image), Slug (Slug), Time, Title (..), Username)
import Domain.Util.Validation (WithUpdate, WithValidation)
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)
import qualified GenUUID (E (Generate))
import qualified Relation.ManyToMany (E (IsRelated))
import qualified Relation.OneToOne (E (GetRelated))
import Relude.Extra (un)
import qualified Storage.Map (E (GetById))
import Validation (Validation (Failure))

type Patchable r =
  ( Coercible (WithUpdate (r "all")) (r "update"),
    Construct WithValidation (r "all"),
    Construct SG.Last (HKD (r "all") WithValidation),
    Construct Maybe (HKD (HKD (r "all") WithValidation) SG.Last),
    Semigroup (WithUpdate (r "all"))
  )

applyPatch ::
  (Patchable r) =>
  r "update" ->
  r "all" ->
  WithValidation (r "all")
applyPatch update orig =
  let orig' = deconstruct $ deconstruct $ deconstruct orig
   in case construct $ orig' <> un update of
        Nothing -> Failure ("impossible: missing field!" :| [])
        Just h -> construct $ SG.getLast $ construct h

class Transform (r :: Symbol -> Type) (s1 :: Symbol) (s2 :: Symbol) (m :: Type -> Type) where
  transform :: (Monad m) => r s1 -> m (r s2)

-- User
instance (HasField "username" (UserR s) Username) => Transform UserR s "id" m where
  transform = pure . UserId . getField @"username"

-- FIXME pw hashing
instance
  ( Algebra sig m,
    Member (Current.E Time) sig,
    Member (Storage.Map.E UserR) sig,
    Member (Relation.OneToOne.E Email "of" (UserR "id")) sig,
    Member (Throw (AlreadyExists Email)) sig,
    Member (Throw (AlreadyExists Username)) sig
  ) =>
  Transform UserR "create" "all" m
  where
  transform (UserRegister user email pw) = do
    void $ send $ GetCurrent @Time
    send (Relation.OneToOne.GetRelated @_ @"of" email) >>= \case
      Just (_ :: UserR "id") -> throwError $ AlreadyExists email
      Nothing ->
        send (Storage.Map.GetById @UserR $ UserId user) >>= \case
          Just _ -> throwError $ AlreadyExists user
          Nothing -> pure $ User email pw user (Bio "") (Image "")

instance Transform UserR "all" "auth" m where
  transform (User email _ name bio image) = pure $ UserAuth email name bio image

instance (Algebra sig m, Member (Authentication.Token.E UserR) sig) => Transform UserR "auth" "authWithToken" m where
  transform auth = UserAuthWithToken auth <$> send (CreateToken auth)

instance (Algebra sig m, Member (Authentication.Token.E UserR) sig) => Transform UserR "all" "authWithToken" m where
  transform = transform >=> (transform @_ @"auth")

instance
  ( Algebra sig m,
    Member (Relation.ManyToMany.E (UserR "id") "follow" (UserR "id")) sig,
    Member (Current.E (UserR "authWithToken")) sig
  ) =>
  Transform UserR "auth" "profile" m
  where
  transform auth = do
    userId <- transform @_ @_ @"id" auth
    cUserId <-
      send (GetCurrent @(UserR "authWithToken"))
        >>= \(UserAuthWithToken auth' _) -> transform @_ @_ @"id" auth'
    following <- send $ Relation.ManyToMany.IsRelated @_ @_ @"follow" cUserId userId
    pure $ UserProfile auth following

-- NOTE: Article

instance {-# OVERLAPPABLE #-} (HasField "slug" (ArticleR s) Slug) => Transform ArticleR s "id" m where
  transform = pure . ArticleId . getField @"slug"

instance {-# OVERLAPPING #-} Transform ArticleR "create" "id" m where
  transform = pure . ArticleId . Slug . Text.intercalate "-" . words . Text.toLower . un . getField @"title"

-- FIXME
instance Transform ArticleR "create" "all" m where
  transform _ = pure undefined

-- FIXME
instance Transform ArticleR "all" "withAuthorProfile" m where
  transform _ = pure undefined

-- NOTE: Comment

instance {-# OVERLAPPABLE #-} (HasField "id" (CommentR s) (CommentR "id")) => Transform CommentR s "id" m where
  transform = pure . getField @"id"

-- FIXME
instance {-# OVERLAPPING #-} (Algebra sig m, Member GenUUID.E sig) => Transform CommentR "create" "id" m where
  transform _ = CommentId <$> send GenUUID.Generate

-- FIXME
instance Transform CommentR "create" "all" m where
  transform _ = pure undefined

-- FIXME
instance Transform CommentR "all" "withAuthorProfile" m where
  transform _ = pure undefined
