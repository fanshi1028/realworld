-- |
module CurrentTime where

import Domain.Util.Field (Time)

data E (m :: Type -> Type) a where
  GetCurrentTime :: E m Time
