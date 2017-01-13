module DOM.ServiceWorker where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

import Network.HTTP.Affjax (URL)
import Unsafe.Coerce as U

foreign import data ActivateEvent :: *
foreign import data ExtendableEvent :: *
foreign import data InstallEvent :: *
foreign import data Registration :: *
foreign import data SERVICE_WORKER :: !
foreign import data SERVICE_WORKER_CLIENT :: !

type ServiceWorkerClientEff e a = Eff (swclient :: SERVICE_WORKER_CLIENT | e) a
type ServiceWorkerEff e a = Eff (sw :: SERVICE_WORKER | e) a

installEventToExtendableEvent :: InstallEvent -> ExtendableEvent
installEventToExtendableEvent = U.unsafeCoerce

activateEventToExtendableEvent :: ActivateEvent -> ExtendableEvent
activateEventToExtendableEvent = U.unsafeCoerce

foreign import onInstall_ :: forall e
  .  (InstallEvent -> ServiceWorkerEff e Unit)
  -> ServiceWorkerEff e Unit

onInstall = onInstall_

foreign import onActivate_ :: forall e
  .  (ActivateEvent -> ServiceWorkerEff e Unit)
  -> ServiceWorkerEff e Unit

onActivate = onActivate_

foreign import waitUntil_ :: forall e a
  . Fn2 ExtendableEvent
        (ServiceWorkerEff e Unit)
        (ServiceWorkerEff e Unit)

waitUntil' :: forall e
  .  ExtendableEvent
  -> ServiceWorkerEff e Unit
  -> ServiceWorkerEff e Unit
waitUntil' = runFn2 waitUntil_

waitUntil :: forall e
  .  ExtendableEvent
  -> ServiceWorkerEff e Unit
  -> ServiceWorkerEff e Unit
waitUntil = waitUntil'

foreign import register_ :: forall e
  . Fn3 URL
        (Error -> ServiceWorkerClientEff e Unit)
        (Registration -> ServiceWorkerClientEff e Unit)
        (ServiceWorkerClientEff e Unit)

register' :: forall e
  .  URL
  -> (Error -> ServiceWorkerClientEff e Unit)
  -> (Registration -> ServiceWorkerClientEff e Unit)
  -> ServiceWorkerClientEff e Unit
register' = runFn3 register_

register :: forall e
  .  URL
  -> Aff (swclient :: SERVICE_WORKER_CLIENT | e) Registration
register = makeAff <<< register'

foreign import claimClients_ :: forall e. ServiceWorkerEff e Unit

claimClients :: forall e. ServiceWorkerEff e Unit
claimClients = claimClients_
