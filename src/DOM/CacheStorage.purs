module DOM.CacheStorage where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.Function.Uncurried (Fn3, runFn3, Fn4, runFn4)

import DOM.Types (URL)

foreign import data CACHE :: !
foreign import data Cache :: *
foreign import data CacheStorage :: *

type CacheName = String

openCache :: forall e
  .  CacheName
  -> Aff (cache :: CACHE | e) Cache
openCache = makeAff <<< openCache'

openCache' :: forall e
  .  CacheName
  -> (Error -> Eff (cache :: CACHE | e) Unit)
  -> (Cache -> Eff (cache :: CACHE | e) Unit)
  -> Eff (cache :: CACHE | e) Unit
openCache' = runFn3 openCache_

foreign import openCache_ :: forall e
  . Fn3 CacheName
        (Error -> Eff (cache :: CACHE | e) Unit)
        (Cache -> Eff (cache :: CACHE | e) Unit)
        (Eff (cache :: CACHE | e) Unit)


foreign import hasCache_ :: forall e
  . Fn3 CacheName
        (Error -> Eff (cache :: CACHE | e) Unit)
        (Boolean -> Eff (cache :: CACHE | e) Unit)
        (Eff (cache :: CACHE | e) Unit)

foreign import deleteCache_ :: forall e
  . Fn3 CacheName
        (Error -> Eff (cache :: CACHE | e) Unit)
        (Boolean -> Eff (cache :: CACHE | e) Unit)
        (Eff (cache :: CACHE | e) Unit)

add :: forall e
  .  Cache
  -> URL
  -> Aff (cache :: CACHE | e) Unit
add c u = makeAff $ add' c u

add' :: forall e
  .  Cache
  -> URL
  -> (Error -> Eff (cache :: CACHE | e) Unit)
  -> (Unit -> Eff (cache :: CACHE | e) Unit)
  -> Eff (cache :: CACHE | e) Unit
add' = runFn4 add_

foreign import add_ :: forall e
  . Fn4 Cache
        URL
        (Error -> Eff (cache :: CACHE | e) Unit)
        (Unit -> Eff (cache :: CACHE | e) Unit)
        (Eff (cache :: CACHE | e) Unit)

addAll :: forall e
  .  Cache
  -> Array URL
  -> Aff (cache :: CACHE | e) Unit
addAll c uu = makeAff $ addAll' c uu

addAll' :: forall e
  .  Cache
  -> Array URL
  -> (Error -> Eff (cache :: CACHE | e) Unit)
  -> (Unit -> Eff (cache :: CACHE | e) Unit)
  -> Eff (cache :: CACHE | e) Unit
addAll' = runFn4 addAll_

foreign import addAll_ :: forall e
  . Fn4 Cache
        (Array URL)
        (Error -> Eff (cache :: CACHE | e) Unit)
        (Unit -> Eff (cache :: CACHE | e) Unit)
        (Eff (cache :: CACHE | e) Unit)
