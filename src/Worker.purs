module Worker where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import DOM.CacheStorage (CACHE, CacheName, openCache, addAll)
import DOM.Event.Types (Event)
import DOM.ServiceWorker (SERVICE_WORKER)

import Network.HTTP.Affjax (URL)

foreign import onInstall :: forall e
  .  (Event -> Eff (console :: CONSOLE, sworker :: SERVICE_WORKER | e) Unit)
  -> Eff (console :: CONSOLE, sworker :: SERVICE_WORKER | e) Unit

cacheName :: CacheName
cacheName = "weather-pwa-v1"

filesToCache :: Array URL
filesToCache =
  [ "/"
  , "/index.html"
  , "/main.js"
  , "/styles/inline.css"
  , "/images/clear.png"
  , "/images/cloudy-scattered-showers.png"
  , "/images/cloudy.png"
  , "/images/fog.png"
  , "/images/ic_add_white_24px.svg"
  , "/images/ic_refresh_white_24px.svg"
  , "/images/partly-cloudy.png"
  , "/images/rain.png"
  , "/images/scattered-showers.png"
  , "/images/sleet.png"
  , "/images/snow.png"
  , "/images/thunderstorm.png"
  , "/images/wind.png"
  , "/jquery-3.1.1.slim.min.js" ]

main :: forall e
  . Eff ( cache :: CACHE
        , console :: CONSOLE
        , err :: EXCEPTION
        , sworker :: SERVICE_WORKER
        | e) Unit
main = do
  onInstall $ \_ -> void $ launchAff $ do
    cache <- openCache cacheName
    addAll cache filesToCache
