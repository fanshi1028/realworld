{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Domain.Util.Representation where

import Control.Algebra (Algebra, send)
import Control.Effect.Sum (Member)
import qualified CurrentTime
import Data.Generic.HKD (Construct (construct, deconstruct), HKD)
import qualified Data.Semigroup as SG
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR (User, UserAuth, UserId))
import Domain.Util.Field (Username (Username))
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)
import Relude.Extra (un)
import Validation (Validation (Failure))
import Validation.Carrier.Selective (WithUpdate, WithValidation)

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

instance (HasField "username" (UserR s) Username) => Transform UserR s "id" m where
  transform = pure . UserId . getField @"username"

-- FIXME
instance (Algebra sig m, Member CurrentTime.E sig) => Transform UserR "create" "all" m where
  transform _ = do
    void $ send CurrentTime.GetCurrentTime
    pure undefined

-- FIXME
instance Transform UserR "all" "authWithToken" m where
  transform _ = pure undefined

instance Transform UserR "all" "auth" m where
  transform (User email _ name bio image _ _) = pure $ UserAuth email name bio image

-- FIXME
instance Transform UserR "all" "profile" m where
  transform _ = pure undefined

-- FIXME
instance Transform UserR "auth" "authWithToken" m where
  transform _ = pure undefined

-- FIXME
instance Transform CommentR "create" "all" m where
  transform _ = pure undefined

-- FIXME
instance Transform CommentR "create" "id" m where
  transform _ = pure undefined

-- FIXME
instance Transform CommentR "all" "withAuthorProfile" m where
  transform _ = pure undefined

-- FIXME
instance Transform CommentR "all" "id" m where
  transform _ = pure undefined

-- FIXME
instance Transform ArticleR "create" "all" m where
  transform _ = pure undefined

-- FIXME
instance Transform ArticleR "all" "id" m where
  transform _ = pure undefined

-- FIXME
instance Transform ArticleR "all" "withAuthorProfile" m where
  transform _ = pure undefined

-- FIXME
instance Transform ArticleR "create" "id" m where
  transform = pure undefined
