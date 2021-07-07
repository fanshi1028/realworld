-- |
module Tag.Effect (Tag (GetTags)) where

import qualified Domain.Util.Field as F (Tag)

data Tag f (m :: Type -> Type) a where
  GetTags :: Tag f m (f F.Tag)
