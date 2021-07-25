{-# LANGUAGE DataKinds #-}

--
module Storage.InMem.STM where

import GHC.TypeLits (Symbol)

-- | NOTE: This split out stm instruction of isntead of the data directly
data E (r :: Symbol -> Type) (m :: Type -> Type) k where
  GetById :: E r m (r "id" -> STM (Maybe (r "all")))
  GetAll :: E r m (STM [r "all"])
  Insert :: E r m (r "id" -> r "all" -> STM (r "all"))
  UpdateById :: E r m (r "id" -> (r "all" -> r "all") -> STM (r "all"))
  DeleteById :: E r m (r "id" -> STM ())
