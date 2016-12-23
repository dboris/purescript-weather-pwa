module Main where

import Prelude hiding (append)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery
  ( JQuery
  , JQueryEvent
  , append
  , appendText
  , body
  , clone
  , create
  , css
  , find
  , getValue
  , on
  , on'
  , ready
  , select
  , setAttr
  , setClass
  , setText
  )
import Control.Monad.Eff.Ref (Ref, REF, readRef, modifyRef, newRef)

import Data.AppState (AppState(..), initialAppState)
import Data.DateTime as DT
import Data.JSDate (LOCALE, parse, toDateTime)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.WeatherCard (WeatherCard, CardKey)

import DOM (DOM)

foreign import test :: Int


main :: forall e. Eff (console :: CONSOLE, dom :: DOM, locale :: LOCALE, ref :: REF | e) Unit
main = do
  log "Hello jQuery!"
  stateRef <- newRef initialAppState
  ready $ do
    addDialog <- select ".dialog-container"
    body <- body
    -- TODO add cardTemplate and container to state
    cardTemplate <- select ".cardTemplate"
    container <- select ".main"
    spinner <- select ".loader"

    on' "click" "#butAdd" (\_ _ -> toggleAddDialog true addDialog) body

    hideSpinner spinner

    -- state <- readRef stateRef
    updateForecastCard cardTemplate container stateRef initialWeatherForecast



-- Methods to update/refresh the UI

hideSpinner :: forall e. JQuery -> Eff (dom :: DOM | e) Unit
hideSpinner = setAttr "hidden" true

toggleAddDialog :: forall e. Boolean -> JQuery -> Eff (dom :: DOM | e) Unit
toggleAddDialog visible = setClass "dialog-container--visible" visible


updateForecastCard
  :: forall e
   . JQuery
  -> JQuery
  -> Ref AppState
  -> WeatherCard
  -> Eff (dom :: DOM, locale :: LOCALE, ref :: REF | e) Unit
updateForecastCard cardTemplate container stateRef cardData = do
  let dataLastUpdated = toDateTime <$> parse cardData.created  -- Maybe DT.DateTime
  let key = cardData.key
  let current = cardData.channel.item.condition

  card <- getOrCreateCard cardTemplate container stateRef key
  -- TODO update card
  find ".description" card >>= setText current.text
  find ".date" card >>= setText current.date
  --     card.querySelector('.current .icon').classList.add(app.getIconClass(current.code));
  find ".current .icon" card >>= setClass "rain" true


getOrCreateCard
  :: forall e
   . JQuery
  -> JQuery
  -> Ref AppState
  -> CardKey
  -> Eff (dom :: DOM, ref :: REF | e) JQuery
getOrCreateCard cardTemplate container stateRef key = do
  AppState state <- readRef stateRef
  case M.lookup key state.visibleCards of
    Just wc ->
      pure wc
    Nothing -> do
      -- card doesn't exist - clone from template and store in state
      nc <- clone cardTemplate
      setClass "cardTemplate" false nc
      setAttr "hidden" false nc
      append nc container
      modifyRef stateRef $ addCardForKey key nc
      pure nc


-- Methods for dealing with the model


addCardForKey :: CardKey -> JQuery -> AppState -> AppState
addCardForKey key card (AppState state) =
  AppState state { visibleCards = M.insert key card state.visibleCards }


-- Fake data
initialWeatherForecast :: WeatherCard
initialWeatherForecast = {
  key: "2459115"
, label: "New York, NY"
, created: "2016-07-22T01:00:00Z"
, channel: {
    astronomy: {
      sunrise: "5:43 am"
    , sunset: "8:21 pm"
    }
  , item: {
      condition: {
        text: "Windy"
      , date: "Thu, 21 Jul 2016 09:00 PM EDT"
      , temp: 56
      , code: 24
      }
    , forecast: [
        {code: 44, high: 86, low: 70}
      , {code: 44, high: 94, low: 73}
      , {code: 4, high: 95, low: 78}
      , {code: 24, high: 75, low: 89}
      , {code: 24, high: 89, low: 77}
      , {code: 44, high: 92, low: 79}
      , {code: 44, high: 89, low: 77}
      ]
    }
  , atmosphere: {humidity: 56}
  , wind: {
      speed: 25
    , direction: 195
    }
  }
}
