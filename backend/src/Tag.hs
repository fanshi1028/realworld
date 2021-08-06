-- |
module Tag (E (GetTags)) where

import Domain.Util.Field (Tag)

data E (m :: Type -> Type) a where
  GetTags :: E m [ Tag ]
