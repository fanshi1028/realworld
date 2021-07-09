-- |
module Tag (E (GetTags)) where

import Domain.Util.Field (Tag)

data E f (m :: Type -> Type) a where
  GetTags :: E f m (f Tag)
