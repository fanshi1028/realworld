{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Storage.Index.InMem where

import Control.Algebra (Algebra (alg), type (:+:) (L, R))
import qualified Focus as FC (Change (Leave, Set), cases, unitCases)
import GHC.TypeLits (Symbol)
import qualified StmContainers.Map as STM (focus, lookup)
import Storage.InMem (TableInMem')
import Storage.Index (E (AddIndex, GetByIndex))

newtype C (r :: Symbol -> Type) (idx :: Symbol) (f :: Type -> Type) m a = C
  { run :: ReaderT (TableInMem' r idx "id") m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TableInMem' r idx "id"))

instance
  ( Show (r idx),
    Eq (r idx),
    Hashable (r idx),
    Algebra sig m,
    MonadIO m
  ) =>
  Algebra (E r idx Maybe :+: sig) (C r idx Maybe m)
  where
  alg _ (L action) ctx =
    ((<$ ctx) <$>) . liftIO . atomically =<< case action of
      GetByIndex idx -> asks $ STM.lookup idx
      AddIndex idx r ->
        asks $
          join
            . STM.focus
              ( FC.cases
                  (pure idx, FC.Set r)
                  (const (throwSTM $ show @Text idx <> "exists" :| [], FC.Leave))
              )
              idx
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx

-- FIXME
instance (Algebra sig m) => Algebra (E r idx [] :+: sig) (C r idx [] m) where
  alg hdl (L (AddIndex idx r)) ctx = undefined
  alg hdl (L (GetByIndex idx)) ctx = undefined
  alg hdl (R other) ctx = C $ alg (run . hdl) (R other) ctx
