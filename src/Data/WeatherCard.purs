module Data.WeatherCard where

import Prelude

import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (defaultOptions, readGeneric)
import Data.Generic (class Generic, gShow)


newtype ForcastData = ForcastData
  { code :: Int
  , high :: Int
  , low :: Int
  }

newtype WeatherCard = WeatherCard
  { key :: String
  , label :: String
  , created :: String
  , channel :: { astronomy :: { sunrise :: String
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
  }

derive instance genericForcastData :: Generic ForcastData
derive instance genericWeatherCard :: Generic WeatherCard

instance showWeatherCard :: Show WeatherCard where
  show = gShow

instance isForeignWeatherCard :: IsForeign WeatherCard where
  read = readGeneric defaultOptions
