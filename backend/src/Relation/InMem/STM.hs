{-# LANGUAGE DataKinds #-}

-- |
module Relation.InMem.STM where

data E r1 r2 (m :: Type -> Type) a where
  Relate :: E r1 r2 m (r1 "id" -> r2 "id" -> STM ())
  Unrelate :: E r1 r2 m (r1 "id" -> r2 "id" -> STM ())
  IsRelated :: E r1 r2 m (r1 "id" -> r2 "id" -> STM Bool)
