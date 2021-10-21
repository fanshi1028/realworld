-- |
-- Description : Effect
-- Copyright   : (c) 2021 fanshi1028
-- Maintainer  : jackychany321@gmail.com
-- Stability   : experimental
--
-- Effect to store data as a map
--
-- @since 0.2.0.0
module Storage.Map (E (..), module X) where

import Storage.Map.Internal.HasCreate as X
import Storage.Map.Internal.HasCreate.Article as X
import Storage.Map.Internal.HasCreate.Comment as X
import Storage.Map.Internal.HasCreate.User as X
import Storage.Map.Internal.HasStorage as X
import Storage.Map.Internal.HasStorage.Article as X
import Storage.Map.Internal.HasStorage.Comment as X
import Storage.Map.Internal.HasStorage.User as X
import Storage.Map.Internal.HasUpdate as X
import Storage.Map.Internal.HasUpdate.Article as X
import Storage.Map.Internal.HasUpdate.User as X

-- | @since 0.2.0.0
data E s (m :: Type -> Type) k where
  -- | Get the data from the storage by its id
  GetById :: IdOf s -> E s m (ContentOf s)
  -- | Get content the data
  GetAll :: E s m [ContentOf s]
  -- | Insert the data
  Insert :: IdOf s -> ContentOf s -> E s m ()
  -- | Update the data in the storage by its id
  UpdateById :: IdOf s -> (ContentOf s -> ContentOf s) -> E s m (ContentOf s)
  -- | Delete the data from the storage by its id
  DeleteById :: IdOf s -> E s m ()
