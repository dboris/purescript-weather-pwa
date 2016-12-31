module Main where

import Prelude hiding (append)

import Control.Comonad (extract)
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.JQuery (JQuery, append, body, clone, find, getText, hide, on', ready, select, setAttr, setClass, setText, toArray)
import Control.Monad.Eff.Now (NOW, nowDate)
import Control.Monad.Eff.Ref (REF, Ref, readRef, modifyRef, newRef)
import Control.Monad.Except (runExcept)

import Data.AppState (AppState(..), CardKey, SelectedCity(..), selectedCitiesKey)
import Data.Array (drop, elem, findIndex, index, singleton, (..))
import Data.Date (Date, weekday)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foreign (F)
import Data.Foreign.Generic (defaultOptions, readJSONGeneric)
import Data.Foreign.WeatherService as WeatherService
import Data.JSDate (LOCALE, parse, toDateTime)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe')
import Data.Tuple.Nested (Tuple3, get1, get2, get3)
import Data.WeatherCard (WeatherCard, ForecastData, fromWeatherService)
import Data.Zippable (zip3)

import DOM (DOM)
import DOM.WebStorage (STORAGE, getItem, setItem, getLocalStorage)
import Partial.Unsafe (unsafePartial)

import Network.HTTP.Affjax (AJAX, get)

foreign import test :: Int


type WeekDay = String

main :: forall e. Eff ( ajax :: AJAX
                      , console :: CONSOLE
                      , dom :: DOM
                      , err :: EXCEPTION
                      , locale :: LOCALE
                      , now :: NOW
                      , ref :: REF
                      , storage :: STORAGE
                      | e ) Unit
main = ready $ do
  spinner <- select ".loader"
  cardTemplate <- select ".cardTemplate"
  container <- select ".main"
  addDialog <- select ".dialog-container"

  body <- body
  on' "click" "#butAdd" (\_ _ -> toggleAddDialog true addDialog) body

  localStorage <- getLocalStorage
  storedSelectedCities <- getItem localStorage selectedCitiesKey

  let selectedCities = fromMaybe' (\_ -> initialSelectedCity) storedSelectedCities

  stateRef <- newRef $ AppState { isLoading: true
                                , visibleCards: Map.empty
                                , selectedCities
                                , spinner
                                , cardTemplate
                                , container
                                , addDialog }

  case storedSelectedCities of
    Just cities ->
      foreachE cities \(SelectedCity { key, label }) -> getForecast stateRef key label
    Nothing -> do
      -- The user is using the app for the first time, or the user has not
      -- saved any cities, so show the user some fake data. A real app in this
      -- scenario could guess the user's location via IP lookup and then inject
      -- that data into the page.
      updateForecastCard stateRef initialWeatherForecast
      saveSelectedCities stateRef


-- Methods to update/refresh the UI --

toggleAddDialog :: forall e. Boolean -> JQuery -> Eff (dom :: DOM | e) Unit
toggleAddDialog visible = setClass "dialog-container--visible" visible

updateForecastCard
  :: forall e
   . Ref AppState
  -> WeatherCard
  -> Eff (console :: CONSOLE, dom :: DOM, locale :: LOCALE, now :: NOW, ref :: REF | e) Unit
updateForecastCard stateRef cardData =
  let key = cardData.key
      current = cardData.channel.item.condition
      forcast = cardData.channel.item.forecast
  in do
    card <- getOrCreateCard stateRef key
    cardLastUpdatedElem <- find "> .card-last-updated" card
    cardLastUpdatedStr <- getText cardLastUpdatedElem

    dataIsOld <- if cardLastUpdatedStr == ""
      then pure Nothing
      else do
        dataLastUpdatedJSDate <- parse cardData.created
        cardLastUpdatedJSDate <- parse cardLastUpdatedStr
        pure $ let dataLastUpdated = unsafePartial $ fromJust (toDateTime dataLastUpdatedJSDate)
                   cardLastUpdated = unsafePartial $ fromJust (toDateTime cardLastUpdatedJSDate)
               in Just (cardLastUpdated > dataLastUpdated)

    let updateCard = do
          setText cardData.created cardLastUpdatedElem
          find "> .location" card >>= setText cardData.label
          find "> .description" card >>= setText current.text
          find "> .date" card >>= setText current.date
          find ".current .icon" card >>= setClass (getIconClass current.code) true
          find ".current .temperature .value" card >>= (setText $ show current.temp)
          find ".current .sunrise" card >>= setText cardData.channel.astronomy.sunrise
          find ".current .sunset" card >>= setText cardData.channel.astronomy.sunset
          find ".current .humidity" card >>= (setText $ show cardData.channel.atmosphere.humidity <> "%")
          find ".current .wind .value" card >>= (setText $ show cardData.channel.wind.speed)
          find ".current .wind .direction" card >>= (setText $ show cardData.channel.wind.direction)

          nextDays <- find ".future .oneday" card >>= toArray
          ndw <- nextDaysOfWeek
          foreachE (zip3 nextDays forcast ndw) updateNextDay

          AppState state <- readRef stateRef
          when state.isLoading do
            hide state.spinner
            modifyRef stateRef \(AppState s) -> AppState s { isLoading = false }

    case dataIsOld of
      Just true -> log $ "Not updating: " <> cardLastUpdatedStr <> ", " <> cardData.created
      Just false -> updateCard
      Nothing -> updateCard

updateNextDay
  :: forall e
   . Tuple3 JQuery ForecastData WeekDay
  -> Eff (dom :: DOM | e) Unit
updateNextDay t =
  let c = get1 t
      daily = get2 t
      wd = get3 t
  in do
    find ".icon" c >>= setClass (getIconClass daily.code) true
    find ".date" c >>= setText wd
    find ".temp-high .value" c >>= (setText $ show daily.high)
    find ".temp-low .value" c >>= (setText $ show daily.low)

getOrCreateCard
  :: forall e
   . Ref AppState
  -> CardKey
  -> Eff (dom :: DOM, ref :: REF | e) JQuery
getOrCreateCard stateRef key = do
  AppState state <- readRef stateRef
  case Map.lookup key state.visibleCards of
    Just card -> pure card
    Nothing -> do
      -- card doesn't exist - clone from template and store in state
      newCard <- clone state.cardTemplate
      setClass "cardTemplate" false newCard
      setAttr "hidden" false newCard
      append newCard state.container
      modifyRef stateRef $ addCardForKey key newCard
      pure newCard


-- Methods for dealing with the model --

-- | Gets a forecast for a specific city and updates the card with the data.
-- | `getForecast` first checks if the weather data is in the cache. If so,
-- | then it gets that data and populates the card with the cached data.
-- | Then, `getForecast` goes to the network for fresh data. If the network
-- | request goes through, then the card gets updated a second time with the
-- | freshest data.
getForecast
  :: forall e
   . Ref AppState
  -> CardKey
  -> String
  -> Eff ( ajax :: AJAX
         , console :: CONSOLE
         , err :: EXCEPTION
         , dom :: DOM
         , locale :: LOCALE
         , now :: NOW
         , ref :: REF
         | e) Unit
getForecast stateRef key label =
  let statement = "select * from weather.forecast where woeid=" <> key
      url = "https://query.yahooapis.com/v1/public/yql?format=json&q=" <> statement
      readOpts = defaultOptions { unwrapSingleConstructors = true }
  in void $ launchAff do
    request <- get url
    case runExcept (readJSONGeneric readOpts request.response :: F WeatherService.Response) of
      Left err -> liftEff $ log (show err)
      Right response ->
        case fromWeatherService key label response of
          Just cardData -> liftEff $ updateForecastCard stateRef cardData
          Nothing -> liftEff $ log "Bad weather service response"

-- | Save list of cities to localStorage.
saveSelectedCities
  :: forall e
   . Ref AppState
  -> Eff (dom :: DOM, ref :: REF, storage :: STORAGE | e) Unit
saveSelectedCities stateRef = do
  AppState state <- readRef stateRef
  localStorage <- getLocalStorage
  setItem localStorage selectedCitiesKey state.selectedCities

today :: forall e. Eff (now :: NOW | e) Date
today = extract <$> nowDate

weekdayFromEnum :: Date -> Int
weekdayFromEnum = fromEnum <<< weekday

daysOfWeek :: Array WeekDay
daysOfWeek = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]

nextDaysOfWeek :: forall e. Eff (now :: NOW | e) (Array WeekDay)
nextDaysOfWeek = do
  d <- weekdayFromEnum <$> today
  pure $ drop d (daysOfWeek <> daysOfWeek)

addCardForKey :: CardKey -> JQuery -> AppState -> AppState
addCardForKey key card (AppState state) =
  AppState state { visibleCards = Map.insert key card state.visibleCards }

-- | Weather codes: https://developer.yahoo.com/weather/documentation.html#codes
weatherCodes :: Array (Array Int)
weatherCodes =
  [ [25, 32, 33, 34, 36, 3200]
  , [0, 1, 2, 6] <> 8..12 <> [17, 35, 40]
  , [3, 4, 37, 38, 39, 45, 47]
  , [5, 7, 13, 14, 16, 18, 41, 42, 43, 46]
  , [15] <> 19..22
  , [23, 24]
  , [26, 27, 28, 31]
  , [29, 30, 44]
  ]

weatherClasses :: Array String
weatherClasses =
  [ "clear-day"
  , "rain"
  , "thunderstorms"
  , "snow"
  , "fog"
  , "windy"
  , "cloudy"
  , "partly-cloudy-day"
  ]

getIconClass :: Int -> String
getIconClass weatherCode =
  fromMaybe "clear-day" $ findIndex (elem weatherCode) weatherCodes >>= index weatherClasses


-- Fake data
initialWeatherForecast :: WeatherCard
initialWeatherForecast =
  { key: "2459115"
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

initialSelectedCity :: Array SelectedCity
initialSelectedCity = singleton $ SelectedCity { key: initialWeatherForecast.key
                                               , label: initialWeatherForecast.label }
