module DOM.CacheStorage where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.Function.Uncurried (Fn3, runFn3, Fn4, runFn4)

import Network.HTTP.Affjax (URL)

foreign import data CACHE :: !
foreign import data Cache :: *
foreign import data CacheStorage :: *

type CacheEff e a = Eff (cache :: CACHE | e) a
type CacheName = String

openCache :: forall e
  .  CacheName
  -> Aff (cache :: CACHE | e) Cache
openCache = makeAff <<< openCache'

openCache' :: forall e
  .  CacheName
  -> (Error -> CacheEff e Unit)
  -> (Cache -> CacheEff e Unit)
  -> CacheEff e Unit
openCache' = runFn3 openCache_

foreign import openCache_ :: forall e
  . Fn3 CacheName
        (Error -> CacheEff e Unit)
        (Cache -> CacheEff e Unit)
        (CacheEff e Unit)

foreign import hasCache_ :: forall e
  . Fn3 CacheName
        (Error -> CacheEff e Unit)
        (Boolean -> CacheEff e Unit)
        (CacheEff e Unit)

foreign import deleteCache_ :: forall e
  . Fn3 CacheName
        (Error -> CacheEff e Unit)
        (Boolean -> CacheEff e Unit)
        (CacheEff e Unit)

add :: forall e
  .  Cache
  -> URL
  -> Aff (cache :: CACHE | e) Unit
add c u = makeAff $ add' c u

add' :: forall e
  .  Cache
  -> URL
  -> (Error -> CacheEff e Unit)
  -> (Unit -> CacheEff e Unit)
  -> CacheEff e Unit
add' = runFn4 add_

foreign import add_ :: forall e
  . Fn4 Cache
        URL
        (Error -> CacheEff e Unit)
        (Unit -> CacheEff e Unit)
        (CacheEff e Unit)

addAll :: forall e
  .  Cache
  -> Array URL
  -> Aff (cache :: CACHE | e) Unit
addAll c uu = makeAff $ addAll' c uu

addAll' :: forall e
  .  Cache
  -> Array URL
  -> (Error -> CacheEff e Unit)
  -> (Unit -> CacheEff e Unit)
  -> CacheEff e Unit
addAll' = runFn4 addAll_

foreign import addAll_ :: forall e
  . Fn4 Cache
        (Array URL)
        (Error -> CacheEff e Unit)
        (Unit -> CacheEff e Unit)
        (CacheEff e Unit)

foreign import deleteOldCaches_ :: forall e
  . Fn3 (Array CacheName)
        (Error -> CacheEff e Unit)
        (Unit -> CacheEff e Unit)
        (CacheEff e Unit)

deleteOldCaches' :: forall e
  .  Array CacheName
  -> (Error -> CacheEff e Unit)
  -> (Unit -> CacheEff e Unit)
  -> CacheEff e Unit
deleteOldCaches' = runFn3 deleteOldCaches_

-- | Delete caches not in the list `currentCacheNames`
deleteOldCaches :: forall e
  .  Array CacheName
  -> Aff ( cache :: CACHE | e) Unit
deleteOldCaches currentCacheNames = makeAff $ deleteOldCaches' currentCacheNames
