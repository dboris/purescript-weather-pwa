module Worker where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import WebWorker (IsWW)


main :: forall e. Eff ( console :: CONSOLE
                      , isww :: IsWW
                      | e) Unit
main =
  log "Worker loaded"
