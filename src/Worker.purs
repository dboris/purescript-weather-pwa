module Worker where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import DOM.CacheStorage (CACHE, CacheName, openCache, addAll)
import DOM.Event.Types (Event)
import DOM.ServiceWorker (SERVICE_WORKER)
import DOM.Types (URL)

foreign import onInstall :: forall e
  .  (Event -> Eff (console :: CONSOLE, sworker :: SERVICE_WORKER | e) Unit)
  -> Eff (console :: CONSOLE, sworker :: SERVICE_WORKER | e) Unit

cacheName :: CacheName
cacheName = "weather-pwa-v1"

filesToCache :: Array URL
filesToCache = ["/styles/inline.css"]

main :: forall e
  . Eff ( cache :: CACHE
        , console :: CONSOLE
        , err :: EXCEPTION
        , sworker :: SERVICE_WORKER
        | e) Unit
main = do
  onInstall $ \e -> void $ launchAff $ do
    cache <- openCache cacheName
    addAll cache filesToCache
