{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
module Util.Orphan where

import Control.Carrier.Error.Either (ErrorC (ErrorC))
import Control.Carrier.Lift (LiftC (LiftC))
import Control.Carrier.Reader (ReaderC (ReaderC))
import Control.Carrier.Throw.Either (ThrowC (ThrowC))
import Control.Exception.Safe (MonadCatch, MonadThrow)

deriving via (ExceptT e m) instance MonadThrow m => MonadThrow (ThrowC e m)

deriving via (ExceptT e m) instance MonadCatch m => MonadCatch (ThrowC e m)

deriving via (ReaderT r m) instance MonadThrow m => MonadThrow (ReaderC r m)

deriving via (ReaderT r m) instance MonadCatch m => MonadCatch (ReaderC r m)

deriving instance MonadThrow m => MonadThrow (LiftC m)

deriving instance MonadCatch m => MonadCatch (LiftC m)
