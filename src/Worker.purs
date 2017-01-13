module Worker where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import DOM.CacheStorage (CACHE, CacheName, addAll, deleteOldCaches, openCache)
import DOM.ServiceWorker (SERVICE_WORKER, onActivate, onInstall, waitUntil, claimClients, activateEventToExtendableEvent, installEventToExtendableEvent)

import Network.HTTP.Affjax (URL)

cacheName :: CacheName
cacheName = "weather-pwa-v1"

dataCacheName :: CacheName
dataCacheName = "weatherData-v1"

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
        , sw :: SERVICE_WORKER
        | e) Unit
main = do
  onInstall $ \event -> do
    log "[ServiceWorker] Install"
    waitUntil (installEventToExtendableEvent event) $ void $ launchAff do
      cache <- openCache cacheName
      addAll cache filesToCache
  onActivate $ \event -> do
    log "[ServiceWorker] Activate"
    waitUntil (activateEventToExtendableEvent event) $ void $ launchAff do
      deleteOldCaches [cacheName, dataCacheName]
    claimClients

