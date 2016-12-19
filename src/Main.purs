module Main where

import Prelude hiding (append)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, on, append, css, create, appendText, body, ready, setText, getValue)

import DOM (DOM)

foreign import test :: Int


main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  log "Hello jQuery!"
  ready $ do
    -- Get the document body
    body <- body

    -- Create a text box
    div   <- create "<div>"
    appendText "Hello PureScript" div
    append div body
