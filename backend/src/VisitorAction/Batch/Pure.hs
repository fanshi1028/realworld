{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module VisitorAction.Batch.Pure (run) where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import Control.Exception.Safe (MonadCatch, MonadThrow)
import qualified Tag (E (GetTags))
import VisitorAction.Batch (E (GetComments, GetTags, ListArticles))

newtype C (f :: Type -> Type) m a = C
  { run :: m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow ,MonadCatch)

instance (Member (Tag.E []) sig, Algebra sig m) => Algebra (VisitorAction.Batch.E [] :+: sig) (C [] m) where
  alg hdl sig ctx = case sig of
    L ListArticles -> pure $ [] <$ ctx
    L (GetComments _) -> pure $ [] <$ ctx
    L GetTags -> (<$ ctx) <$> send Tag.GetTags
    R other -> C $ alg (run . hdl) other ctx
