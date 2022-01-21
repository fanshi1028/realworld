{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Effect & Carrier
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect and Carrier for sql using 'Rel8'
--
-- @since 0.4.0.0
module InRel8.Sql where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Effect.Lift (Lift, sendM)
import Control.Effect.Sum (Member)
import Control.Effect.Throw (Throw, throwError)
import qualified Control.Effect.Trace as Trace
import Data.Util.Impossible (Impossible (Impossible))
import Hasql.Statement (Statement (Statement))
import Hasql.Transaction (Transaction, statement)
import Rel8
  ( Delete (Delete),
    Expr,
    Insert (Insert),
    Name,
    OnConflict (DoNothing),
    Returning (NumberOfRowsAffected, Projection),
    Serializable,
    TableSchema,
    Transposes,
    Update (Update),
    delete,
    insert,
    showDelete,
    showInsert,
    showUpdate,
    update,
    values,
  )

-- | @since 0.4.0.0
data SqlInRel8E (m :: Type -> Type) a where
  -- | @since 0.4.0.0
  SqlSelect :: Statement () a -> SqlInRel8E m a
  -- | @since 0.4.0.0
  SqlInsert :: Insert a -> SqlInRel8E m a
  -- | @since 0.4.0.0
  SqlUpdate :: Update a -> SqlInRel8E m a
  -- | @since 0.4.0.0
  SqlDelete :: Delete a -> SqlInRel8E m a

-- | @since 0.4.0.0
newtype SqlInRel8TransactionC m a = SqlInRel8TransactionC
  { runSqlInRel8Transaction :: m a
  }
  deriving (Functor, Applicative, Monad)

-- | @since 0.4.0.0
instance
  (Algebra sig m, Member (Lift Transaction) sig, Member Trace.Trace sig) =>
  Algebra (SqlInRel8E :+: sig) (SqlInRel8TransactionC m)
  where
  alg _ (L action) ctx =
    (<$ ctx) <$> case action of
      SqlSelect s@(Statement sql _ _ _) -> Trace.trace (decodeUtf8 sql) >> sendM (statement () s)
      SqlInsert i -> Trace.trace (showInsert i) >> sendM (statement () $ insert i)
      SqlUpdate u -> Trace.trace (showUpdate u) >> sendM (statement () $ update u)
      SqlDelete d -> Trace.trace (showDelete d) >> sendM (statement () $ delete d)
  alg hdl (R others) ctx = SqlInRel8TransactionC $ alg (runSqlInRel8Transaction . hdl) others ctx
  {-# INLINE alg #-}

-- | @since 0.4.0.0
toggleOn ::
  ( Algebra sig m,
    Member (Throw Impossible) sig,
    Member SqlInRel8E sig,
    Transposes Name Expr names exprs
  ) =>
  TableSchema names ->
  exprs ->
  m b ->
  m b
toggleOn schema row after = do
  n <-
    send . SqlInsert $
      Insert
        schema
        (values [row])
        DoNothing
        NumberOfRowsAffected
  if n > 1
    then throwError $ Impossible "got more than one row inserted"
    else after
{-# INLINE toggleOn #-}

-- | @since 0.4.0.0
toggleOff ::
  ( Algebra sig m,
    Member (Throw Impossible) sig,
    Member SqlInRel8E sig,
    Transposes Name Expr names rows
  ) =>
  TableSchema names ->
  (rows -> Expr Bool) ->
  m ()
toggleOff schema p = do
  n <-
    send . SqlDelete $
      Delete
        schema
        (pure ())
        (\_ r -> p r)
        NumberOfRowsAffected
  when (n > 1) $ throwError $ Impossible "got more than one row removed"
{-# INLINE toggleOff #-}

-- | @since 0.4.0.0
insertRows ::
  ( Algebra sig m,
    Member SqlInRel8E sig,
    Foldable f,
    Serializable exprs returning,
    Transposes Name Expr names rows
  ) =>
  TableSchema names ->
  f rows ->
  (rows -> exprs) ->
  m [returning]
insertRows schema rows proj =
  send . SqlInsert $
    Insert
      schema
      (values rows)
      DoNothing
      $ Projection proj
{-# INLINE insertRows #-}

-- | @since 0.4.0.0
insertOneRow ::
  ( Algebra sig m,
    Member (Throw Impossible) sig,
    Member SqlInRel8E sig,
    Serializable exprs returning,
    Transposes Name Expr names rows
  ) =>
  TableSchema names ->
  rows ->
  (rows -> exprs) ->
  m returning ->
  m returning
insertOneRow schema row proj noInsert =
  send
    ( SqlInsert $
        Insert
          schema
          (values [row])
          DoNothing
          $ Projection proj
    )
    >>= \case
      [] -> noInsert
      [c] -> pure c
      _ -> throwError $ Impossible "got more than one row inserted"
{-# INLINE insertOneRow #-}

-- | @since 0.4.0.0
deleteOneRow ::
  ( Algebra sig m,
    Member (Throw Impossible) sig,
    Member SqlInRel8E sig,
    Serializable exprs returning,
    Transposes Name Expr names rows
  ) =>
  TableSchema names ->
  (rows -> Expr Bool) ->
  (rows -> exprs) ->
  m (Maybe returning)
deleteOneRow schema f proj =
  send
    ( SqlDelete $
        Delete
          schema
          (pure ())
          (\_ r -> f r)
          $ Projection proj
    )
    >>= \case
      [] -> pure Nothing
      [c] -> pure $ Just c
      _ -> throwError $ Impossible "got more than one row deleted"
{-# INLINE deleteOneRow #-}

-- | @since 0.4.0.0
updateOneRow ::
  ( Algebra sig m,
    Member (Throw Impossible) sig,
    Member SqlInRel8E sig,
    Serializable exprs returning,
    Transposes Name Expr names exprs
  ) =>
  TableSchema names ->
  (exprs -> exprs) ->
  (exprs -> Expr Bool) ->
  m (Maybe returning)
updateOneRow schema f p =
  send
    ( SqlUpdate $
        Update
          schema
          (pure ())
          (\_ r -> f r)
          (\_ r -> p r)
          $ Projection Prelude.id
    )
    >>= \case
      [] -> pure Nothing
      [c] -> pure $ Just c
      _ -> throwError $ Impossible "got more than one row updated"
{-# INLINE updateOneRow #-}
