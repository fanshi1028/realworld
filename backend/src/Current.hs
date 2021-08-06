-- |
module Current where

data E (e :: Type) (m :: Type -> Type) a where
  GetCurrent :: E e m e
