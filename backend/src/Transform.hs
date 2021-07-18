{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module Transform where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import Domain.Article (ArticleR)
import Domain.Comment (CommentR)
import Domain.User (UserR (UserRegister, User))
import GHC.TypeLits (Symbol)

data E (r :: Symbol -> Type) (s1 :: Symbol) (s2 :: Symbol) (m :: Type -> Type) a where
  Transform :: r s1 -> E r s1 s2 m (r s2)

newtype C (r :: Symbol -> Type) (s1 :: Symbol) (s2 :: Symbol) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO)

-- FIXME
instance (Algebra sig m) => Algebra (E UserR "create" "all" :+: sig) (C UserR "create" "all" m) where
  alg _ (L (Transform (UserRegister user em pass))) ctx = pure $ undefined <$ ctx
  -- UserRegister {username, email, password} = do
  --   send GetCurrentTime
  --   pure $ User email password username undefined
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m) => Algebra (E UserR "all" "authWithToken" :+: sig) (C UserR "all" "authWithToken" m) where
  alg _ (L (Transform User {..})) ctx = undefined
    -- send
  -- transform User {..} = UserAuth email token username bio image
    -- pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m) => Algebra (E UserR "all" "profile" :+: sig) (C UserR "all" "profile" m) where
  alg _ (L (Transform ur)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m) => Algebra (E UserR "auth" "authWithToken" :+: sig) (C UserR "auth" "authWithToken" m) where
  alg _ (L (Transform ur)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m) => Algebra (E CommentR "create" "all" :+: sig) (C CommentR "create" "all" m) where
  alg _ (L (Transform ur)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m) => Algebra (E CommentR "all" "withAuthorProfile" :+: sig) (C CommentR "all" "withAuthorProfile" m) where
  alg _ (L (Transform ur)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m) => Algebra (E ArticleR "all" "withAuthorProfile" :+: sig) (C ArticleR "all" "withAuthorProfile" m) where
  alg _ (L (Transform ur)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx

-- FIXME
instance (Algebra sig m) => Algebra (E ArticleR "create" "all" :+: sig) (C ArticleR "create" "all" m) where
  alg _ (L (Transform ur)) ctx = pure $ undefined <$ ctx
  alg hdl (R other) ctx = C $ alg (run . hdl) other ctx
