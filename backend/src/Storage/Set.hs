-- |
module Storage.Set where

data E (e :: Type) (m :: Type -> Type) k where
  IsMember :: e -> E e m Bool
  GetAll :: E e m [e]
  Insert :: e -> E e m ()
  Delete :: e -> E e m ()
