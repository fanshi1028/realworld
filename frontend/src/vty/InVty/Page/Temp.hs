-- | @since 0.4.0.0
module InVty.Page.Temp where

import InVty.Util (Go)
import Reflex (Event, never)
import Reflex.Vty (HasDisplayRegion, HasImageWriter, HasTheme, text)
import Reflex.Workflow (Workflow)

-- | @since 0.4.0.0
tempPage ::
  (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) =>
  (Event t Go -> Event t (Workflow t m (Event t a))) ->
  Text ->
  m (Event t a, Event t (Workflow t m (Event t a)))
tempPage router tag = do
  text $ pure $ "under construction: " <> tag
  pure (never, router never)
