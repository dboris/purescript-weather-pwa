module Data.WeatherCard where

import Prelude (bind, pure)

import Data.AppState (CardKey)
import Data.Foreign.WeatherService
import Data.Int as Int
import Data.Maybe (Maybe)


type ForcastData =
  { code :: Int
  , high :: Int
  , low :: Int
  }

type WeatherChannel =
  { astronomy :: { sunrise :: String
                 , sunset :: String
                 }
  , item :: { condition :: { text :: String
                           , date :: String
                           , temp :: Int
                           , code :: Int
                           }
            , forecast :: Array ForcastData
            }
  , atmosphere :: { humidity :: Int }
  , wind :: { speed :: Int
            , direction :: Int
            }
  }

type WeatherCard =
  { key :: CardKey
  , label :: String
  , created :: String
  , channel :: WeatherChannel
  }

fromWeatherService :: CardKey -> String -> Response -> Maybe WeatherCard
fromWeatherService key label (Response { query }) =
  case query of
    Query { created, results } ->
      case results of
        Results { channel } ->
          case channel of
            Channel { astronomy, item, atmosphere, wind } ->
              case astronomy of
                Astronomy { sunrise, sunset } ->
                  case item of
                    Item { condition, forecast } ->
                      case condition of
                        Condition { text, date, temp, code } ->
                          case atmosphere of
                            Atmosphere { humidity } ->
                              case wind of
                                Wind { speed, direction } -> do
                                  temp' <- Int.fromString temp
                                  code' <- Int.fromString code
                                  humidity' <- Int.fromString humidity
                                  speed' <- Int.fromString speed
                                  direction' <- Int.fromString direction
                                  pure { key
                                       , label
                                       , created
                                       , channel: { astronomy: { sunrise
                                                               , sunset
                                                               }
                                                  , item: { condition: { text
                                                                       , date
                                                                       , temp: temp'
                                                                       , code: code'
                                                                       }
                                                          , forecast: []
                                                          }
                                                  , atmosphere: { humidity: humidity' }
                                                  , wind: { speed: speed'
                                                          , direction: direction'
                                                          }
                                                  }
                                       }

