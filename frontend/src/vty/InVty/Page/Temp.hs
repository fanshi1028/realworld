-- | @since 0.4.0.0
module InVty.Page.Temp where

import Reflex (Event, never)
import Reflex.Vty (HasDisplayRegion, HasImageWriter, HasTheme, text)
import Reflex.Workflow (Workflow (Workflow))

-- | @since 0.4.0.0
tempPage ::
  (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) =>
  Text ->
  Event t (Workflow t m (Event t a)) ->
  Workflow t m (Event t a)
tempPage tag next = Workflow $ do
  text $ pure $ "under construction: " <> tag
  pure (never, next)
