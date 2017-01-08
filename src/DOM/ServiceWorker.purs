module DOM.ServiceWorker where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.Function.Uncurried (Fn3, runFn3, Fn4, runFn4)

import DOM.Types (URL)

foreign import data Registration :: *
foreign import data SERVICE_WORKER :: !
foreign import data SERVICE_WORKER_CLIENT :: !

foreign import register_ :: forall e
  . Fn3 URL
        (Error -> Eff (swclient :: SERVICE_WORKER_CLIENT | e) Unit)
        (Registration -> Eff (swclient :: SERVICE_WORKER_CLIENT | e) Unit)
        (Eff (swclient :: SERVICE_WORKER_CLIENT | e) Unit)

register' :: forall e
  .  URL
  -> (Error -> Eff (swclient :: SERVICE_WORKER_CLIENT | e) Unit)
  -> (Registration -> Eff (swclient :: SERVICE_WORKER_CLIENT | e) Unit)
  -> Eff (swclient :: SERVICE_WORKER_CLIENT | e) Unit
register' = runFn3 register_

register :: forall e
  .  URL
  -> Aff (swclient :: SERVICE_WORKER_CLIENT | e) Registration
register = makeAff <<< register'

