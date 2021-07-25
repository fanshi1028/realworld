{-# LANGUAGE DataKinds #-}

-- |
module Relation where

data E r1 r2 (m :: Type -> Type) a where
  Relate :: r1 "id" -> r2 "id" -> E r1 r2 m ()
  Unrelate :: r1 "id" -> r2 "id" -> E r1 r2 m ()
  IsRelated :: r1 "id" -> r2 "id" -> E r1 r2 m Bool
