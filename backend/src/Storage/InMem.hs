{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Storage.InMem where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Sum (Member)
import GHC.TypeLits (Symbol)
import qualified GenID (E (GenerateID))
import qualified StmContainers.Map as STM (Map)
import Storage (E (DeleteById, GetAll, GetById, Insert, UpdateById))
import qualified Storage.STM as STM (E (DeleteById, GetAll, GetById, Insert, UpdateById))
import qualified Transform (E (Transform))

type TableInMem' r (k :: Symbol) (v :: Symbol) = STM.Map (r k) (r v)

type TableInMem r = TableInMem' r "id" "all"

newtype C (r :: Symbol -> Type) m a = C
  { run :: ReaderT (TableInMem r) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TableInMem r))

instance
  ( Show (r "id"),
    Eq (r "id"),
    Hashable (r "id"),
    Member (Transform.E r "create" "all") sig,
    Member (GenID.E r) sig,
    Member (STM.E r) sig,
    Member (Transform.E r "create" "all") sig,
    Algebra sig m,
    MonadIO m
  ) =>
  Algebra (E r :+: sig) (C r m)
  where
  alg hdl sig ctx = case sig of
    (L action) ->
      (<$ ctx)
        <$> ( ( case action of
                  GetById id' -> send STM.GetById <*> pure id'
                  GetAll -> send STM.GetAll
                  Insert create -> send STM.Insert <*> send (GenID.GenerateID create) <*> send (Transform.Transform @_ create)
                  UpdateById id' updateF -> send STM.UpdateById <*> pure id' <*> pure updateF
                  DeleteById id' -> send STM.DeleteById <*> pure id'
              )
                >>= liftIO . atomically
            )
    (R other) -> C $ alg (run . hdl) (R other) ctx
